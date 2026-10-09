
def bufsize : USize := 20 * 1024

def help_message : String :=
"Usage: feline [--help] [FILE]...
Concatenate FILE(s) to standard output.

With no FILE, or when FILE is -, read standard input.
Arguments:
  -h, --help, display this message and exit
"
partial def dump (stream: IO.FS.Stream) : IO Unit := do
  let buf <- stream.read bufsize
  if buf.isEmpty then
    pure ()
  else
    let stdout <- IO.getStdout
    stdout.write buf
    dump stream

def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists <- filename.pathExists
  if not fileExists then
    let stderr <- IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle <- IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

def process (exitCode : UInt32) (args: List String) : IO UInt32 := do
  match args with
  | [] => pure exitCode
  | "-" :: args =>
    let stdin <- IO.getStdin
    dump stdin
    process exitCode args
  | filename :: args =>
    let stream <- fileStream filename
    match stream with
    | none => process 1 args
    | some stream =>
      dump stream
      process exitCode args

def main (args : List String) : IO UInt32 :=
  if List.contains args "--help" || List.contains args "-h" then
    do
      IO.print help_message
      return 0
  else match args with
  | [] => process 0 ["-"]
  | _ => process 0 args
