param([string]$day)

$folderName = "day-${day}"
$solutionName = "Day${day}"
$projectName = "Day${day}"
$testProjectName = "Day${day}.Tests"

mkdir $folderName

dotnet new sln -n $solutionName -o $folderName

dotnet new console -lang "F#" -o "${folderName}/${projectName}"
dotnet new nunit -lang "F#" -o "${folderName}/${testProjectName}"

dotnet add "${folderName}/${testProjectName}" reference "${folderName}/${projectName}"

dotnet sln "${folderName}/${solutionName}.sln" add "${folderName}/${projectName}/${projectName}.fsproj"
dotnet sln "${folderName}/${solutionName}.sln" add "${folderName}/${testProjectName}/${testProjectName}.fsproj"

New-Item "${folderName}/${projectName}/input.txt"