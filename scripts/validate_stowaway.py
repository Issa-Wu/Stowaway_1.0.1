# External authority check for stowaway benchmark outputs.
# Usage: python3 scripts/validate_stowaway.py [DIR]   (default: the dir printed
# by scripts/benchmark.R, or "." if you copied the files somewhere)
#
# Python stdlib zipfile reads every entry, decompresses it and verifies the
# CRC32 recorded in the central directory: testzip() returns the first bad
# entry, None means the whole archive is intact.
import sys, zipfile, hashlib, os

base = sys.argv[1] if len(sys.argv) > 1 else "."
files = {
    "A_5mb.pptx": ("ppt/media/payload.bin", "big_random.bin"),
    "B_nested.pptx": ("ppt/media/nested.pptx", None),
    "C_batch.pptx": ("ppt/media/c01.csv", None),
    "D_level_b.pptx": ("ppt/media/reg.csv", None),
}

for name, (part, orig) in files.items():
    path = os.path.join(base, name)
    if not os.path.exists(path):
        print(f"SKIP {name}: not found in {base}")
        continue
    z = zipfile.ZipFile(path)
    bad = z.testzip()
    infos = z.infolist()
    ct = sum(1 for i in infos if i.filename == "[Content_Types].xml")
    print(f"{name}: entries={len(infos)}, testzip bad entry={bad}, CT count={ct}")
    if orig:
        with z.open(part) as f:
            got = hashlib.md5(f.read()).hexdigest()
        want = hashlib.md5(open(os.path.join(base, orig), "rb").read()).hexdigest()
        print(f"  payload md5 match: {got == want} ({got[:12]}...)")
