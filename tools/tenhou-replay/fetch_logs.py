#!/usr/bin/env python3
"""Download Tenhou mjlog zips linked from the ranking page and unzip them.

    fetch_logs.py [--mode 4p|3p|both] [--count N] [--out DIR]

The ranking page (default https://tenhou.net/ranking.html) links DL zips
like /0/log/mjlog_pf4-20_n30.zip (4-player houou) and
/0/log/mjlog_pf3-20_n31.zip (3-player houou). By default the latest 4-player
zip is fetched, matching what convert.py accepts (4-player hanchan with red
fives and open tanyao); --mode 3p or both also fetches sanma logs, which
convert.py reports and skips.

Extracted .mjlog/.xml files can be fed to convert.py:

    python3 tools/tenhou-replay/fetch_logs.py --mode 4p
    python3 tools/tenhou-replay/convert.py tools/tenhou-replay/logs/ tools/tenhou-replay/converted/
    CL_MAHJONG_REPLAY_DIR=tools/tenhou-replay/converted/ sbcl --noinform --non-interactive \\
        --eval '(asdf:test-system "cl-mahjong")'

Only the standard library is used.
"""
import argparse
import re
import sys
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path

DEFAULT_RANKING_URL = "https://tenhou.net/ranking.html"
DEFAULT_OUT = Path(__file__).resolve().parent / "logs"

# href="/0/log/mjlog_pf4-20_n30.zip" (4p) or mjlog_pf3-... (3p)
ZIP_LINK = re.compile(r'href="([^"]*mjlog_pf([34])[^"]*\.zip)"')

USER_AGENT = "cl-mahjong-fetch-logs/1.0 (+https://github.com/takeiteasy/cl-mahjong)"


def find_zips(html, base_url):
    """Unique zip URLs in page order (newest first) as (url, mode) pairs."""
    seen, found = set(), []
    for match in ZIP_LINK.finditer(html):
        url = urllib.parse.urljoin(base_url, match.group(1))
        mode = "4p" if match.group(2) == "4" else "3p"
        if url not in seen:
            seen.add(url)
            found.append((url, mode))
    return found


def select_zips(zips, mode, count):
    """The COUNT newest zips per selected mode, preserving page order."""
    wanted = {"4p", "3p"} if mode == "both" else {mode}
    picked, taken = [], {m: 0 for m in wanted}
    for url, zip_mode in zips:
        if zip_mode in wanted and taken[zip_mode] < count:
            picked.append((url, zip_mode))
            taken[zip_mode] += 1
    return picked


def fetch_text(url):
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(request) as response:
        raw = response.read()
    for encoding in ("utf-8", "shift_jis", "euc-jp"):
        try:
            return raw.decode(encoding)
        except UnicodeDecodeError:
            continue
    return raw.decode("utf-8", errors="replace")


def download(url, dest, force):
    if dest.exists() and not force:
        print(f"exists, skipping download: {dest.name}")
        return dest
    print(f"downloading {url} -> {dest}")
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    with urllib.request.urlopen(request) as response, open(dest, "wb") as out:
        total = response.getheader("Content-Length")
        total = int(total) if total else 0
        done, last_pct = 0, -1
        while True:
            chunk = response.read(1024 * 256)
            if not chunk:
                break
            out.write(chunk)
            done += len(chunk)
            if total:
                pct = done * 100 // total
                if pct != last_pct and pct % 10 == 0:
                    print(f"  {pct}% ({done}/{total} bytes)")
                    last_pct = pct
    print(f"  saved {dest} ({dest.stat().st_size} bytes)")
    return dest


def unzip(zip_path, out_dir, max_logs):
    extracted = 0
    with zipfile.ZipFile(zip_path) as archive:
        members = [m for m in archive.infolist()
                   if not m.is_dir() and Path(m.filename).suffix in (".mjlog", ".xml")]
        if max_logs > 0:
            members = members[:max_logs]
        for member in members:
            target = out_dir / Path(member.filename).name
            if target.exists():
                continue
            with archive.open(member) as src, open(target, "wb") as dst:
                dst.write(src.read())
            extracted += 1
    print(f"extracted {extracted} logs from {zip_path.name} (new files; existing kept)")
    return extracted


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--ranking-url", default=DEFAULT_RANKING_URL,
                        help="page to scrape DL links from")
    parser.add_argument("--mode", choices=("both", "4p", "3p"), default="4p",
                        help="which houou logs to fetch (default: 4p, the only mode convert.py accepts)")
    parser.add_argument("--count", type=int, default=1,
                        help="newest zips to fetch per mode (default: 1)")
    parser.add_argument("--out", type=Path, default=DEFAULT_OUT,
                        help="directory for zips and extracted logs")
    parser.add_argument("--list-only", action="store_true",
                        help="print selected zip URLs without downloading")
    parser.add_argument("--no-unzip", action="store_true",
                        help="download zips without extracting logs")
    parser.add_argument("--no-keep-zips", action="store_true",
                        help="delete each zip after extracting")
    parser.add_argument("--max-logs", type=int, default=0,
                        help="extract at most N logs per zip (0 = all)")
    parser.add_argument("--force", action="store_true",
                        help="re-download zips that already exist")
    args = parser.parse_args()

    if args.count < 1:
        print("error: --count must be >= 1", file=sys.stderr)
        return 2

    print(f"fetching ranking page: {args.ranking_url}")
    try:
        html = fetch_text(args.ranking_url)
    except OSError as error:
        print(f"error: cannot fetch ranking page: {error}", file=sys.stderr)
        return 1
    zips = find_zips(html, args.ranking_url)
    if not zips:
        print("error: no mjlog zip links found on the ranking page", file=sys.stderr)
        return 1
    selected = select_zips(zips, args.mode, args.count)
    if not selected:
        print("error: no zips match the selected mode", file=sys.stderr)
        return 1

    for url, mode in selected:
        print(f"[{mode}] {url}")
    if args.list_only:
        return 0

    args.out.mkdir(parents=True, exist_ok=True)
    total = 0
    for url, _mode in selected:
        zip_path = args.out / Path(urllib.parse.urlparse(url).path).name
        try:
            download(url, zip_path, args.force)
            if not args.no_unzip:
                total += unzip(zip_path, args.out, args.max_logs)
                if args.no_keep_zips:
                    zip_path.unlink()
        except (OSError, zipfile.BadZipFile) as error:
            print(f"error: {url}: {error}", file=sys.stderr)
            return 1
    print(f"done: {len(selected)} zip(s), {total} new log(s) in {args.out}")
    print(f"next: python3 tools/tenhou-replay/convert.py {args.out}/ <converted-dir>/")
    return 0


if __name__ == "__main__":
    sys.exit(main())
