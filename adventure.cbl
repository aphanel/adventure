       >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. adventure.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT GameStateFile ASSIGN TO "gamestate"
                     ORGANIZATION IS LINE SEQUENTIAL
                     ACCESS MODE IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD GameStateFile.
        01 Protagonist.
           02 Blase PIC X(18).
           02 Strength PIC 99.
           02 Vitality PIC 99.
           02 Dexterity PIC 99.

       WORKING-STORAGE SECTION.
        01 WSProtagonist.
           02 WSBlase PIC X(18) VALUE 'Unknown'.
           02 WSStrength PIC 99 VALUE 01.
           02 WSVitality PIC 99 VALUE 01.
           02 WSDexterity PIC 99 VALUE 01.
        01 Action PIC X(16).
        01 Choice PIC 9.
        01 Place PIC X(16).
        01 GameLoop PIC X VALUE "Y".
        01 CanGo PIC X VALUE "N".
        01 Seed PIC 9(6).
        01 RandVal PIC 9(2) VALUE ZERO.
       
       PROCEDURE DIVISION.
       Incipit.
           PERFORM ResetCharacter.
           ACCEPT Seed FROM TIME.
           DISPLAY "You wake up in a haze, feeling cold and achy.".
           DISPLAY "As you look around, you realize that you are in the middle of a forest but you don't remember how you got there.".
           DISPLAY "What do you do? (type help to get a list of commands)".
           PERFORM UNTIL GameLoop="N"
               DISPLAY " "
               ACCEPT Action
               DISPLAY " "
                   EVALUATE Action
                       WHEN "help" PERFORM Help
                       WHEN "look" PERFORM Look
                       WHEN "quit" PERFORM Quit
                       WHEN "go" PERFORM GoToPlace
                       WHEN OTHER PERFORM OtherAction
                   END-EVALUATE
               DISPLAY "What do you do?"
           END-PERFORM.
           STOP RUN.

       Help.
           DISPLAY "You may type the following keyword: ".
           DISPLAY "look".
           DISPLAY "quit".
           IF CanGo="Y"
               DISPLAY "go"
           END-IF.
       
       Look.
           DISPLAY "It is dark but the moon is almost full tonight.".
           DISPLAY "And after some time, you can see your surroundings pretty clearly.".
           DISPLAY "Not so far away, a dancing light pierces through the bushes.".
           DISPLAY "Maybe you could go there..."
           MOVE "Y" TO CanGo.

       Quit.
           MOVE "N" TO GameLoop.
           STOP RUN.
          
       GoToPlace.
           IF CanGo="Y"
               DISPLAY "where do you want to go?"
               PERFORM ListPlaces
               ACCEPT Choice
               PERFORM ComputePlace
           ELSE PERFORM OtherAction       
           END-IF.

       OtherAction.
           DISPLAY "Your meaningless attempt at life proves inconsequential".

       ListPlaces.
           DISPLAY "   1- to the bushes".

       ComputePlace.
           IF Choice="1"
               PERFORM Bushes
           ELSE DISPLAY "You walk a bit and find yourself where you started."       
           END-IF.

       Bushes.
           DISPLAY "As you approach, the source of the light becomes apparent: a bonfire.".
           DISPLAY "The warmth of the fire calms you a little. For a moment, you get lost in a thought".
           DISPLAY "as you remember a childhood memory you hadn't had in a long time..".
           DISPLAY " ".
           PERFORM ChildhoodMemory.
       
       ChildhoodMemory.
           DISPLAY "You are..".
           DISPLAY "   1- in an alley".
           DISPLAY "   2- by a lake".
           ACCEPT Choice.
           IF Choice="1"
               DISPLAY "You are in an alley"
               OPEN OUTPUT GameStateFile                       
                   MOVE 15 TO Dexterity
                   WRITE Protagonist
                   END-WRITE
               CLOSE GameStateFile
           END-IF.
           IF Choice="2"
               DISPLAY "You are by a lake."
               OPEN OUTPUT GameStateFile                   
                   MOVE 15 TO Strength
                   WRITE Protagonist
                   END-WRITE
               CLOSE GameStateFile
           END-IF.

       ResetCharacter.
           OPEN OUTPUT GameStateFile               
               MOVE WSBlase TO Blase                   
               MOVE WSDexterity TO Dexterity
               MOVE WSStrength TO Strength
               MOVE WSVitality TO Vitality
               WRITE Protagonist
               END-WRITE
           CLOSE GameStateFile.

       RollDice.
           COMPUTE RandVal = FUNCTION RANDOM (Seed) * 20 + 1.
