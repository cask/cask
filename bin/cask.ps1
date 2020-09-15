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
        $script:exitStatus = $?
   } finally {
        $Env:EMACSLOADPATH = $emacsLoadPath
        $Env:Path = $path
    }
}

$exitStatus = $true
$caskCmd = $args[0]

# For behavior when accessing nonexistent indices, see:
# https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-arrays#off-by-one-errors

if ($caskCmd -eq "emacs") {
    $rest = $args[1..$args.Length]
    Exec-Command $args[0] @rest
} elseif ($caskCmd -eq "exec") {
    $rest = $args[2..$args.Length]
    Exec-Command $args[1] @rest
} else {
    Exec-Cask $args
}

# Return $? of $true for nonzero [int] or $false
if (-not $exitStatus) {throw "Bad exit"}