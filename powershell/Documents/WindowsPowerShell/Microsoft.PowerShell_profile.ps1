Set-PSReadlineOption -EditMode vi

Set-Alias g git
# C:\WINDOWS\system32\find.exe overrides scoop shim,
# fix by explicitly aliasing.
Set-Alias find find.ps1
# Someone else aliases ls to Get-ChildItem, override that.
Remove-Item alias:ls
function ls { ls.exe --color=yes $args }
Set-Alias l ls
function ll { ls -l $args }
function la { ls --almost-all $args }
function lla { ls -l --almost-all $args }
# Remove alias for curl to Invoke-WebRequest
Remove-Item alias:curl
# Remove alias for cat to Get-Content
Remove-Item alias:cat

Set-PSReadlineKeyHandler -Chord Ctrl+P -Function PreviousHistory
Set-PSReadlineKeyHandler -Chord Ctrl+N -Function NextHistory
Set-PSReadlineKeyHandler -Chord Ctrl+R -Function ReverseSearchHistory
Set-PSReadlineKeyHandler -Key Tab -Function Complete

function make-link ($target, $link) {
    New-Item -Path $link -ItemType SymbolicLink -Value $target
}

function Prompt
{
    $mywd = (Get-Location).Path
    $mywd = $mywd.Replace( $HOME, '~' )
    Write-Host ($mywd + ">") -NoNewline -ForegroundColor Green
    return " "
}
