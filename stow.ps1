function dodiff($file1, $file2)
{
    if (!($str1 = $(Get-Content $file1 -ErrorAction SilentlyContinue)))
    {
        Write-Host $file1 does not exist
        return
    }
    if (!($str2 = $(Get-Content $file2 -ErrorAction SilentlyContinue)))
    {
        Write-Host $file2 copied
        Copy-Item -Path $file1 -Destination $file2
        return
    }
    if (Compare-Object -ReferenceObject $str1 -DifferenceObject $str2)
    {
        Write-Host $file2 differs
        & nvim.exe -d $file1 $file2
        return
    }
    Write-Host $file2 up to date
}

dodiff "nvim\.config\nvim\init.vim" "$HOME\AppData\Local\nvim\init.vim"
dodiff "git\.config\git\config" "$HOME\.gitconfig"
dodiff "powershell\Documents\PowerShell\Microsoft.PowerShell_profile.ps1" "$HOME\Documents\PowerShell\Microsoft.PowerShell_profile.ps1"
dodiff "tig\.config\tig\config" "$HOME\.tigrc"
