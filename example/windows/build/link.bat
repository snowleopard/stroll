if exist bin\Main.o (
    ghc bin\Main.o -o bin\HelloWorld.exe 
) else (
    touch bin\Main.o
    echo Error: bin\Main.o is missing
    exit 1
)