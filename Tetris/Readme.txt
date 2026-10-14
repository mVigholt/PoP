1. Install the .NET SDK
The project targets .NET 10.0 and uses the DIKU.Canvas library, version 1.0.1.
If you haven't installed .NET, download the SDK from:
https://dotnet.microsoft.com/download

You can check whether it is installed by opening PowerShell or the VS Code terminal and running:
	dotnet --version


2. Run the game
Extract tetris.zip into a folder, open that folder in VS Code, and run:
	dotnet restore
	dotnet run

Alternatively, run the project directly from another directory:
	dotnet run --project "C:\path\to\tetris\tetris.fsproj"


The game should open in a graphical window titled Tetris if the project builds successfully and the Canvas library works with your installed .NET runtime.

3. Controls
Key		Action
Left arrow	Move left
Right arrow	Move right
Down arrow	Move down
Up arrow	Rotate piece
Space		Restart game