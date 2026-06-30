import re
from pathlib import Path

# folder
folder = Path("/Users/alexander/Downloads/drive-download-20260630T162623Z-3-001")

# srt
timestamp_pattern = re.compile(
    r"(\d{2}:\d{2}:\d{2}),(\d{3})"
)

for srt_file in folder.glob("*.srt"):
    vtt_file = srt_file.with_suffix(".vtt")

    with open(srt_file, "r", encoding="utf-8-sig") as f:
        content = f.read()

    # timestamp format
    content = timestamp_pattern.sub(r"\1.\2", content)

    # WEBVTT header
    vtt_content = "WEBVTT\n\n" + content

    with open(vtt_file, "w", encoding="utf-8") as f:
        f.write(vtt_content)

    print(f"Converted: {srt_file.name} -> {vtt_file.name}")

print("Done")