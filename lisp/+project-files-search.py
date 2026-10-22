"""Forward existing Emacs sessions to the relocated search worker."""

from pathlib import Path
import runpy

runpy.run_path(
    str(Path(__file__).resolve().parent / "project-files" / "+project-files-search.py"),
    run_name="__main__",
)
