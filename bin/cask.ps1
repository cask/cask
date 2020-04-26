function Exec-Cask {
    emacs -Q --batch --script "$Env:UserProfile\.cask\cask-cli.el" -- @args
}

function Exec-Command($cmd) {
    $emacsLoadPath = $Env:EMACSLOADPATH
    $path = $Env:Path
    try {
        $Env:EMACSLOADPATH = "$(Exec-Cask load-path)"
        $Env:Path = "$(Exec-Cask path)"
        & $cmd @args
   } finally {
        $Env:EMACSLOADPATH = $emacsLoadPath
        $Env:Path = $path
    }
}

function Exec-Emacs {
    Exec-Command emacs @args
}

$caskCmd = $args[0]

if ($caskCmd -eq "emacs") {
    Exec-Emacs $args[1..($args.Count - 1)]
} elseif ($caskCmd -eq "exec") {
    Exec-Command $args[1..($args.Count - 1)]
} else {
    Exec-Cask $args
}
