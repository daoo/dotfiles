Set-PSReadlineOption -EditMode vi

Set-Alias g git

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
