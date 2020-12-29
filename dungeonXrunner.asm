	.inesprg 1   ; 1x 16KB PRG code
	.ineschr 1   ; 1x  8KB CHR data
	.inesmap 0   ; mapper 0 = NROM, no bank swapping
	.inesmir 1   ; background mirroring

;;;;;;;;;;;;;;; Hier Die Variablen

      .rsset $0000
gamestate .rs 1  ; der status des Spiels
gamestateChange .rs 1 ; =1 if room changed
buttons .rs 1    ; die gedrückten Knöpfe sind wie folgt in 8 bit: A B select start up down left right

playerTopLeftX .rs 1 ;Spielerposition x (obere linke ecke des sprites)   ; hier die Variablen für Collision
playerTopLeftY .rs 1 ;Spielerposition x (obere linke ecke des sprites)
playerCollisionTopLeftX .rs 1
playerCollisionTopLeftY .rs 1
playerCollisionTopRightX .rs 1
playerCollisionTopRightY .rs 1
playerCollisionLeftMiddleX .rs 1
playerCollisionLeftMiddleY .rs 1
playerCollisionRightMiddleX .rs 1
playerCollisionRightMiddleY .rs 1
playerCollisionBotLeftX .rs 1
playerCollisionBotLeftY .rs 1
playerCollisionBotRightX .rs 1
playerCollisionBotRightY .rs 1
playerCollides .rs 1
playerInRow .rs 1
collisionPointToCheckX .rs 1     ;der Punkt, der gerade überprüft wird (x und y)
collisionPointToCheckY .rs 1

playerACounter .rs 1 ;ein counter um die sprites zu wechseln
playerSecondSpriteCounter .rs 1 ; noch ein indikator für die sprites
playerDirection .rs 1  ;Offet für Spriteberechnung
playerDirectionLast .rs 1 ;last direction for the attack
playerAttackEndurance .rs 1 
playerProjectileCooldown .rs 1
playerProj0 .rs 1
playerProjectile0Direction .rs 1
offsetEnemyProjectileCollision .rs 1
bufferPlayerTopLeftX .rs 1
bufferPlayerTopLeftY .rs 1
Column .rs 1 ; die spalte
playerLifeCount .rs 1   ; amount of lifepoints for player
lifeLossCooldown .rs 1  ; the invincibility frames after being hit

roomInitialized .rs 1 ; check if the entered room is initialized
entityDirection .rs 1 ; used for generating projectiles
enemy0Direction .rs 1 ; 0 for up, 1 for down
enemy0MovingPause .rs 1
enemy0Proj0 .rs 1
enemy0Projectile0Direction .rs 1

enemy1Direction .rs 1
enemy1MovingPause .rs 1
enemy1Proj0 .rs 1
enemy1Proj1 .rs 1
enemy1Projectile0Direction .rs 1
enemy1Projectile1Direction .rs 1

spacing0 .rs 4 ; bugfix

bugfix .rs 8

enemy2Direction .rs 1
enemy2Proj0 .rs 1
enemy2MovingPause .rs 1
enemy2Projectile0Direction .rs 1

projectileTopLeftY .rs 1  ; temp variables for projectiles
projectileTopLeftX .rs 1
projectileDirection .rs 1
projectileSpriteAddr .rs 1 ; projectile address ($0200 + address) 
collisionHappened .rs 1  ; if 1 projectile gets set to 0 (non existent)

entityTopLeftY .rs 1
entityTopLeftX .rs 1
offsetEnemyCollision .rs 1

overwritePlayerProjectile .rs 1
playerProj1 .rs 1
playerProjectile1Direction .rs 1

L_byte .rs 1  ; used for game_over_screen
H_byte .rs 1  ;

;;;;;;;;;;;;;;;;Hier die Konstanten

Room01 = $01
Room02 = $02

PPUCTRL = $2000
PPUMASK = $2001
PPUSTATUS = $2002
OAMADDR = $2003
OAMDATA = $2004 ;nicht benutzt
PPUSCROLL = $2005
PPUADDR = $2006
PPUDATA = $2007
OAMDMA = $4014

FALSE = $00
TRUE = $01

;;;;;;;;;;;;;;;;Hier beginnt Part 1 - das Hochfahren und der Startbildschirm;;;;;;;

	.bank 0
	.org $C000

;;;;; Famitone Datein einbinden
    .include "famitone2.asm"
	.include "kidicarus.asm"
	.include "SFX.asm"
    .include "zeldadungeon.asm"
vblankwait:     ; wait for vblank to make sure PPU is ready
	BIT PPUSTATUS
	BPL vblankwait
	RTS
 
RESET:
	SEI          ; disable IRQs
	CLD          ; disable decimal mode
	LDX #$40
	STX $4017    ; disable APU frame IRQ
	LDX #$FF
	TXS          ; Set up stack
	INX          ; now X = 0
	INY
	STX PPUCTRL  ; disable NMI
	STX PPUMASK  ; disable rendering
	STX $4010    ; disable DMC IRQs

	JSR vblankwait
 
clrmem:
	LDA #$00
	STA $0000, x
	STA $0100, x
	STA $0300, x
	STA $0400, x
	STA $0500, x
	STA $0600, x
	STA $0700, x
	LDA #$FE
	STA $0200, x
	INX
	BNE clrmem

	JSR vblankwait

;;;;;;;Die Farbpalette wird geladen

LoadPalettes:
	LDA PPUSTATUS         ; read PPU status to reset the high/low latch
	LDA #$3F
	STA PPUADDR           ; write the high byte of $3F00 address
	LDA #$00
	STA PPUADDR           ; write the low byte of $3F00 address
	LDX #$00              ; start out at 0
LoadPalettesLoop:
	LDA palette, x        ; load data from address (palette + the value in x)
	STA PPUDATA           ; write to PPU
	INX                   ; X = X + 1
	CPX #$20              ; Compare X to hex $20, decimal 32 - copying 32 bytes
	BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
	                      ; if compare was equal to 32, keep going down

;;;;;;Als nächstes wird der Hintergrund von Raum 01 geladen

LoadBackground:
	LDA PPUSTATUS         ; read PPU status to reset the high/low latch
	LDA #%00000100   ; vertical drawing
	STA PPUCTRL
	LDA #$00
	STA Column
.loadNextColumn:
    LDA #$20
    STA PPUADDR  ; high byte of address
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR ; low byte of address
    LDX #$00
.load1:
    LDA Column
    CMP #$00
    BEQ .loadDoor
    CMP #$1E
    BEQ .loadDoor
    CMP #$08
    BEQ .loadPillars1
    CMP #$16
    BEQ .loadPillars1
    CMP #$0E
    BEQ .loadFloor
    CMP #$10
    BEQ .loadFloor
.loadTopBotWall:
    JSR LoadTopBotWall
    JMP .correctColumnLoaded
.loadFloor:
    JSR LoadFloorOnly
    JMP .correctColumnLoaded
.loadDoor:
    JSR LoadDoorLeftRight
    JMP .correctColumnLoaded
.loadPillars1:
    JSR LoadPillars1
    JMP .correctColumnLoaded
.correctColumnLoaded:
	LDA Column
	CMP #$1F
	BEQ .backgroundDone
	CLC
	ADC #$01
	STA Column
	JMP .loadNextColumn
.backgroundDone:


;;;;;;;;Die Attributstabelle wird geladen

LoadAttribute:
	LDA PPUSTATUS          ; read PPU status to reset the high/low latch
	LDA #%00000000   ; vertical drawing
	STA PPUCTRL
	LDA #$23
	STA PPUADDR            ; write the high byte of $23C0 address
	LDA #$C0
	STA PPUADDR            ; write the low byte of $23C0 address
	LDX #$00               ; start out at 0
LoadAttributeLoop:
	LDA attribute, x       ; load data from address (attribute + the value in x)
	STA PPUDATA            ; write to PPU
	INX                    ; X = X + 1
	CPX #$40               ; Compare X to hex $40, decimal 64 - copying 64 bytes
	BNE LoadAttributeLoop  ; Branch to LoadAttributeLoop if compare was Not Equal to zero
                           ; if compare was equal to 128, keep going down

;;;;;Sound Dateien geladen

    LDA $00
    ;LDX #LOW(zeldadungeon_music_data)
	;LDY #HIGH(zeldadungeon_music_data)

    LDX #LOW(kidicarus_music_data)      ;song, den man will auskommentieren
	LDY #HIGH(kidicarus_music_data)
	JSR FamiToneInit

	LDA #0
	JSR FamiToneMusicPlay

	LDX #LOW(sounds)
	LDY #HIGH(sounds)
	JSR FamiToneSfxInit


;;;;;Spieler wird geladen

LoadPlayer:
    LDA #$78               ; platziere Spieler Mittig
    STA playerTopLeftX
    LDA #$70
    STA playerTopLeftY
    LDX #$00
    LoadPlayerLoop:
    LDA playerInit, x
    STA $0200, x
    INX
    CPX #$10  ;16 byte für 4 sprites
    BNE LoadPlayerLoop

;;;;;;;;;;Startvariablen?

    LDA #Room01
    STA gamestate
    LDA #$10
    STA playerACounter
    LDA #$03
    STA playerLifeCount
    
;;;;;;;;;;;Sobald alles bereit ist, werden NMIs aktiviert, das heißt, es wird erst jetzt etwas auf dem Bildschirm angezeigt

	LDA PPUSTATUS
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	STA PPUCTRL
	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA PPUMASK

;;;;;;;;;;;Ab hier ist das aufwärmprogramm fertig, alles weitere wird über NMIs geregelt
Forever:
	JMP Forever      ;jump back to Forever, infinite loop, waiting for NMI

;;;;;;;;;;;;;;ab hier NMI, dieses wird durch den Beginn eines neuen VBlanks ausgelöst

NMI:
    LDA #$00
    STA $2003       ; set the low byte (00) of the RAM address
    LDA #$02
    STA $4014       ; set the high byte (02) of the RAM address, start the transfer

    JSR FamiToneUpdate  ;Update famitone every frame
    JSR ReadController1
    
GameEngine:
    LDA gamestate
    CMP #Room01
    BEQ GameRoom01JMP
    CMP #Room02
    BEQ GameRoom02JMP
    JMP GameRoom03JMP
GameEngineDone:

    JSR UpdateSprites
    JSR ClearSprites    
    JSR CheckCollisionEnemys
    JSR MainProjectileFunction
    JSR HandleLifeCount
    
NoScrolling:
  LDA #$00
  STA $2005
  STA $2005

  RTI   ; return from Interrupt, NMI over


;;;;;jumps to the rooms

GameRoom01JMP:
    JMP GameRoom01

GameRoom02JMP:
    JMP GameRoom02

GameRoom03JMP:
    JMP GameRoom03

;;;;;the rooms

GameRoom01:

.initialize:
    LDA roomInitialized
    CMP #$01
    BNE .dostuff
    JMP .initialized

.dostuff:
    LDA PPUSTATUS
    LDA %00000000
    STA PPUCTRL
    LDA %00000000
    STA PPUMASK

.loadRoom01Background:
	LDA PPUSTATUS         ; read PPU status to reset the high/low latch
	LDA #%00000100   ; vertical drawing
	STA PPUCTRL
	LDA #$00
	STA Column
.loadNextColumn:
    LDA #$20
    STA PPUADDR  ; high byte of address
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR ; low byte of address
    LDX #$00
.load1:
    LDA Column
    CMP #$00
    BEQ .loadDoor
    CMP #$1E
    BEQ .loadDoor
    CMP #$08
    BEQ .loadPillars1
    CMP #$16
    BEQ .loadPillars1
    CMP #$0E
    BEQ .loadFloor
    CMP #$10
    BEQ .loadFloor
.loadTopBotWall:
    JSR LoadTopBotWall
    JMP .correctColumnLoaded
.loadFloor:
    JSR LoadFloorOnly
    JMP .correctColumnLoaded
.loadDoor:
    JSR LoadDoorLeftRight
    JMP .correctColumnLoaded
.loadPillars1:
    JSR LoadPillars1
    JMP .correctColumnLoaded
.correctColumnLoaded:
	LDA Column
	CMP #$1F
	BEQ .backgroundDone
	CLC
	ADC #$01
	STA Column
	JMP .loadNextColumn
.backgroundDone:

    LDA #$1D
    STA enemy0Direction
    LDA #$1B
    STA enemy1Direction
    LDA #$1B
    STA enemy2Direction
    LDX #$00
.loadEnemy0Loop:
    LDA enemy0Init, x
    STA $0270, x
    INX
    CPX #$10  ;16 byte für 4 sprites
    BNE .loadEnemy0Loop
    LDX #$00
.loadEnemy1Loop:
    LDA enemy1Init, x
    STA $0280, x
    INX
    CPX #$10
    BNE .loadEnemy1Loop
    LDX #$00
.loadEnemy2Loop:
    LDA enemy2Init, x
    STA $0290, x
    INX
    CPX #$10
    BNE .loadEnemy2Loop

	LDA #$01
	STA roomInitialized
	LDA PPUSTATUS
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	STA PPUCTRL
	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA PPUMASK

.initialized
.enemy0:
    LDA $0271                         ; if enemy is deactivated dont move 
    CMP #$FF
    BNE .enemy0Down
    JMP .enemy1
.enemy0Down:
    LDX #$01                          ; moves every second time
    CPX enemy0MovingPause
    BNE .enemy0MovingPause
    LDX #$00 
    STX enemy0MovingPause
    LDA $0273
    STA entityTopLeftX
    LDA $0270
    STA entityTopLeftY
    LDA enemy0Direction
    CMP #$1D                          ; check if enemy is on its way up
    BEQ .enemy0Up
    LDA entityTopLeftY
    CLC
    ADC #$01
    STA entityTopLeftY
    CMP #$96                          ; if y-pos == $96 walk up
    BNE .enemy0Update
    LDA #$1D
    STA enemy0Direction
    JMP .enemy0Update
.enemy0Up
    LDA entityTopLeftY
    CMP #$90
    BNE .shootProjectileDone
.shootProjectile:                      ; shoot projectile at pos #$90
    LDA enemy0Direction                ; set the variables for the projectile
    STA enemy0Projectile0Direction
    STA entityDirection
    LDA #$40
    STA projectileSpriteAddr
    JSR GenerateProjectile
    LDA #$01
    STA enemy0Proj0
.shootProjectileDone:
    LDA entityTopLeftY
    SEC
    SBC #$01
    STA entityTopLeftY
    CMP #$32                           ; if y-pos == $32 walk down
    BNE .enemy0Update
    LDA #$1B
    STA enemy0Direction
.enemy0Update:                         ; update the pos
    LDA entityTopLeftY
    STA $0270
    STA $0274
    CLC
    ADC #$08
    STA $0278
    STA $027C
    JMP .enemy1
.enemy0MovingPause:                    ; enemy moves next time
    LDX enemy0MovingPause
    INX
    STX enemy0MovingPause
    
.enemy1:
    LDA $0281
    CMP #$FF
    BNE .enemy1Down
    JMP .enemy2
.enemy1Down:
    LDA $0283
    STA entityTopLeftX
    LDA $0280
    STA entityTopLeftY
    LDA enemy1Direction
    CMP #$1D              ; check if enemy is on its way up
    BEQ .enemy1Up
    LDA entityTopLeftY
    CLC
    ADC #$01
    STA entityTopLeftY
    CMP #$C0
    BEQ .shootProjectile10
    JMP .shootProjectile10Done
.shootProjectile10:
    LDA #$01
    STA enemy1Projectile0Direction
    STA entityDirection
    LDA #$44
    STA projectileSpriteAddr
    JSR GenerateProjectile
    LDA #$01
    STA enemy1Proj0
.shootProjectile10Done:
    LDA entityTopLeftY
    CMP #$C0              ; if y-pos == $C0 walk up
    BNE .enemy1Update
    LDA #$1D
    STA enemy1Direction
    JMP .enemy1Update
.enemy1Up
    LDA entityTopLeftY
    SEC
    SBC #$01
    STA entityTopLeftY
    CMP #$20
    BEQ .shootProjectile11
    JMP .shootProjectile11Done
.shootProjectile11:
    LDA #$01
    STA enemy1Projectile1Direction
    STA entityDirection
    LDA #$48
    STA projectileSpriteAddr
    JSR GenerateProjectile
    LDA #$01
    STA enemy1Proj1
.shootProjectile11Done:
    LDA entityTopLeftY
    CMP #$20               ; if y-pos == $20 walk down
    BNE .enemy1Update
    LDA #$1B
    STA enemy1Direction
.enemy1Update:
    LDA entityTopLeftY
    STA $0280
    STA $0284
    CLC
    ADC #$08
    STA $0288
    STA $028C
    JMP .enemy2

.enemy2:
    LDA $0291
    CMP #$FF
    BNE .enemy2Down
    JMP .gameRoom01Done
.enemy2Down:
    LDX #$01            ; moves every second time
    CPX enemy2MovingPause
    BNE .enemy2MovingPauseJMP
    LDX #$00 
    STX enemy2MovingPause
    LDA $0293
    STA entityTopLeftX
    LDA $0290
    STA entityTopLeftY
    LDA enemy2Direction
    CMP #$01            ; check if enemy is walking to the right
    BEQ .enemy2Right
    CMP #$1D            ; check if enemy is walking up
    BEQ .enemy2Up
    CMP #$00            ; check if enemy is walking to the left
    BEQ .enemy2Left
    CMP #$1B            ; check if enemy is walking down
    LDA entityTopLeftY
    CLC
    ADC #$01
    STA entityTopLeftY
    CMP #$86            ; if Y = 86 walk right 
    BNE .enemy2Update
    LDA #$01
    STA enemy2Direction
    JMP .enemy2Update
.enemy2MovingPauseJMP:
    JMP .enemy2MovingPause
.enemy2Right:
    LDA entityTopLeftX
    CLC
    ADC #$01
    STA entityTopLeftX
    CMP #$A0             ; if X = A0 walk up 
    BNE .enemy2Update
    LDA #$1D
    STA enemy2Direction
    JMP .enemy2Update
.enemy2Up:
    LDA entityTopLeftY
    SEC
    SBC #$01
    STA entityTopLeftY
    CMP #$58              ; if Y = 58 walk left
    BNE .enemy2Update
    LDA #$00
    STA enemy2Direction
    JMP .enemy2Update
.enemy2Left:
    LDA entityTopLeftX
    SEC
    SBC #$01
    STA entityTopLeftX
    CMP #$50              ; if X = 50 walk down
    BNE .enemy2Update
    LDA #$1B
    STA enemy2Direction
    JMP .enemy2Update
.enemy2Update:
    LDA entityTopLeftX
    CMP #$78
    BNE .shootProjectile2Done
.shootProjectile2:
    LDA enemy2Direction
    STA enemy2Projectile0Direction
    STA entityDirection
    LDA #$4C
    STA projectileSpriteAddr
    JSR GenerateProjectile
    LDA #$01
    STA enemy2Proj0
.shootProjectile2Done:
    LDA entityTopLeftY  ; change the Y-Pos
    STA $0290
    STA $0294
    CLC
    ADC #$08
    STA $0298
    STA $029C
    LDA entityTopLeftX  ; change the X-Pos
    STA $0293
    STA $029B
    CLC
    ADC #$08
    STA $0297
    STA $029F
    JMP .gameRoom01Done
.enemy2MovingPause:
    LDX enemy2MovingPause
    INX
    STX enemy2MovingPause

.gameRoom01Done:
    JMP Controls

GameRoom02:
    LDA roomInitialized
    CMP #$01
    BNE .dostuff
    JMP .initialized

.dostuff:
    LDA PPUSTATUS
    LDA %00000000
    STA PPUCTRL
    LDA %00000000
    STA PPUMASK

.loadRoom02Background:
	LDA PPUSTATUS         ; read PPU status to reset the high/low latch
	LDA #%00000100   ; vertical drawing
	STA PPUCTRL
	LDA #$00
	STA Column
.loadNextColumn:
    LDA #$20
    STA PPUADDR  ; high byte of address
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR ; low byte of address
    LDX #$00
.load1:
    LDA Column
    CMP #$00
    BEQ .loadDoor
    CMP #$1E
    BEQ .loadDoor
    CMP #$06
    BEQ .loadBigPillarLL
    CMP #$12
    BEQ .loadBigPillarLL
    CMP #$08
    BEQ .loadBigPillarLM
    CMP #$14
    BEQ .loadBigPillarLM
    CMP #$0A
    BEQ .loadBigPillarRM
    CMP #$16
    BEQ .loadBigPillarRM
    CMP #$0C
    BEQ .loadBigPillarRR
    CMP #$18
    BEQ .loadBigPillarRR
    CMP #$0E
    BEQ .loadBotDoor
    CMP #$10
    BEQ .loadBotDoor
.loadTopBotWall:
    JSR LoadTopBotWall
    JMP .correctColumnLoaded
.loadBotDoor:
    JSR LoadDoorBot
    JMP .correctColumnLoaded
.loadDoor:
    JSR LoadDoorLeftRight
    JMP .correctColumnLoaded
.loadPillars1:
    JSR LoadPillars1
    JMP .correctColumnLoaded
.loadBigPillarLL
    JSR LoadBigPillarLeftLeft
    JMP .correctColumnLoaded
.loadBigPillarLM
    JSR LoadBigPillarLeftMiddle
    JMP .correctColumnLoaded
.loadBigPillarRM
    JSR LoadBigPillarRightMiddle
    JMP .correctColumnLoaded
.loadBigPillarRR
    JSR LoadBigPillarRightRight
    JMP .correctColumnLoaded
.correctColumnLoaded:
	LDA Column
	CMP #$1F
	BEQ .backgroundDone
	CLC
	ADC #$01
	STA Column
	JMP .loadNextColumn
.backgroundDone:

	LDA #$01
	STA roomInitialized
	LDA PPUSTATUS
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	STA PPUCTRL
	LDA #%00011110   ; enable sprites, enable background, no clipping on left side
	STA PPUMASK

.initialized:

.gameRoom02Done:
    JMP Controls

GameRoom03:
    ;room
    ;design
.gameRoom03Done:
    JMP Controls


GameOverScreen:
    LDA roomInitialized
    CMP #$01
    BEQ .initialized
    LDA PPUSTATUS
    LDA %00000000
    STA PPUCTRL
    LDA %00000000
    STA PPUMASK

.gameOverbackground:
    LDA #$20
	STA PPUADDR         ; write the high byte of $2000 address
	LDA #$00
	STA PPUADDR         ; write the low byte of $2000 address
	LDX #$00            ; start out at 0
	LDY #$00
	LDA #low(gameOverScreen)
	STA L_byte
	LDA #high(gameOverScreen)
	STA H_byte
.loop:
	LDA [L_byte], y     ; load data from address (background-startzeile + the value in y)
	STA PPUDATA         ; write to PPU
	INY                 ; Y = Y + 1
	CPY #$00            ; compare X to hex $00, decimal 256(0) - copying 256 bytes
	BNE .loop 			; branch to LoadBackgroundLoop if compare was Not Equal to zero if compare was equal to 256, keep going down
	INC H_byte
	INX
	CPX #$04
	BNE .loop
.backgroundDone:

	LDA #$01
	STA roomInitialized

	LDA PPUSTATUS
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
	STA PPUCTRL
	LDA #%00001110   ; enable sprites, enable background, no clipping on left side
	STA PPUMASK

.initialized

    LDA buttons
    AND #%00010000
    BEQ .screenDone
    JMP RESET
.screenDone:
    JMP GameEngineDone



;;;;; Player controls

Controls:

HandleA:
    LDX playerProjectileCooldown     ; cooldown for projectile gets reduced
    CPX #$00
    BEQ .start
    DEX
    STX playerProjectileCooldown
.start:
    LDX playerAttackEndurance
    LDA buttons
    AND #%10000000                   ; A pressed
    BEQ ADone
    CPX #$10                         ; if endurance at $10, cant attack
    BEQ .rightDoneJMP
    INX
    STX playerAttackEndurance
    LDA #$FF                         ; $FF stands for attacking
    STA playerDirection

	LDA #0
    LDX #FT_SFX_CH1                  ;shooting sound
    JSR FamiToneSfxPlay 

.rightDoneJMP:
    JMP MovementDone
ADone:
    LDX playerAttackEndurance        ; cooldown for attack gets reduced
    CPX #$00
    BEQ .end
    DEX
    .end:
    STX playerAttackEndurance

HandleUp:
    LDA buttons
    AND #%00001000 ; up pressed
    BNE .up  ; up not pressed -> jump to end
    JMP UpDone
.up:
    LDA #$0A
    STA playerDirection
    LDA #$1D
    STA playerDirectionLast

.isCounterZero:     ; animation
    LDA playerACounter
    CMP #$00
    BEQ .isZeroOrOne
    CMP #$01
    BEQ .isZeroOrOne
    SEC
    SBC #$02
    STA playerACounter
.isZeroOrOne:          ; animation end
    LDA playerTopLeftY
    CMP #$08
    BEQ .changeRoomUp
    SEC
    SBC #$01
    STA playerTopLeftY
.terrainCollisionRoom01
    JSR UpdateTopCollisionPoints         ; update the Points that shouls bechecked for collision
    LDA playerCollisionTopLeftX          ; check top left first
    STA collisionPointToCheckX
    LDA playerCollisionTopLeftY
    STA collisionPointToCheckY
    JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
    LDA playerCollides
    CMP TRUE
    BNE .checkSecond
    LDA playerTopLeftY                 ; if collision, then dont store new position
    CLC
    ADC #$01
    STA playerTopLeftY
    JMP .jumpToEnd
.checkSecond:
     LDA playerCollisionTopRightX          ; check top left first
     STA collisionPointToCheckX
     LDA playerCollisionTopRightY
     STA collisionPointToCheckY
     JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
     LDA playerCollides
     CMP TRUE
     BNE .jumpToEnd
     LDA playerTopLeftY                 ; if collision, then dont store new position
     CLC
     ADC #$01
     STA playerTopLeftY
     JMP .jumpToEnd
.changeRoomUp:
    LDA gamestate
    CMP #$01
    BEQ .jumpToEnd ; BEQ uses relative addressing and has 1 byte for the difference (Branch address out of reach)
    CMP #$02
    BEQ .jumpToEnd
    CMP #$03
    BEQ .jumpToEnd
    SEC
    SBC #$03
    STA gamestate
    LDA #$D8
    STA playerTopLeftY
    LDA #$01
    STA gamestateChange ; sets room change bit, so sprites can be cleared

	LDA #3
    LDX #FT_SFX_CH0     ;entering new room sound
    JSR FamiToneSfxPlay 

.jumpToEnd:
    JMP MovementDone
UpDone:
 
HandleDown:
    LDA buttons
    AND #%00000100 ; down pressed
    BNE .down    ; 4-way-movement- keine diabonalen bewegungen !!!
    JMP DownDone
.down:
    LDA #$1B
    STA playerDirectionLast

.isCounterZero:
    LDA playerACounter
    CMP #$00
    BEQ .isZeroOrOne
    CMP #$01
    BEQ .isZeroOrOne
    SEC
    SBC #$02
    STA playerACounter
.isZeroOrOne:
    LDA playerTopLeftY
    CMP #$D8
    BEQ .changeRoomDown
    CLC
    ADC #$01
    STA playerTopLeftY
.terrainCollisionRoom01
    JSR UpdateBotCollisionPoints         ; update the Points that shouls bechecked for collision
    LDA playerCollisionBotLeftX          ; check top left first
    STA collisionPointToCheckX
    LDA playerCollisionBotLeftY
    STA collisionPointToCheckY
    JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
    LDA playerCollides
    CMP TRUE
    BNE .checkSecond
    LDA playerTopLeftY                 ; if collision, then dont store new position
    SEC
    SBC #$01
    STA playerTopLeftY
    JMP .jumpToEnd
.checkSecond:
     LDA playerCollisionBotRightX          ; check top left first
     STA collisionPointToCheckX
     LDA playerCollisionBotRightY
     STA collisionPointToCheckY
     JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
     LDA playerCollides
     CMP TRUE
     BNE .jumpToEnd
     LDA playerTopLeftY                 ; if collision, then dont store new position
     SEC
     SBC #$01
     STA playerTopLeftY
     JMP .jumpToEnd
.changeRoomDown:
    LDA gamestate
    CMP #$07
    BEQ .jumpToEnd
    CMP #$08
    BEQ .jumpToEnd
    CMP #$09
    BEQ .jumpToEnd
    CLC
    ADC #$03
    STA gamestate
    LDA #$08
    STA playerTopLeftY
    LDA #$01
    STA gamestateChange

	LDA #3
    LDX #FT_SFX_CH1     ;entering new room sound
    JSR FamiToneSfxPlay 

.jumpToEnd:
    JMP MovementDone
DownDone:

HandleLeft:
    LDA buttons
    AND #%00000010 ; left pressed
    BNE .left
    JMP LeftDone
.left:
    LDA #$06
    STA playerDirection
    LDA #$00
    STA playerDirectionLast
.isCounterZero:
    LDA playerACounter
    CMP #$00
    BEQ .isZeroOrOne
    CMP #$01
    BEQ .isZeroOrOne
    SEC
    SBC #$02
    STA playerACounter
.isZeroOrOne:
    LDA playerTopLeftX
    CMP #$00
    BEQ .changeRoomLeft
    SEC
    SBC #$01
    STA playerTopLeftX
.terrainCollisionRoom01
    JSR UpdateLeftCollisionPoints
    LDA playerCollisionTopLeftX
    STA collisionPointToCheckX
    LDA playerCollisionTopLeftY
    STA collisionPointToCheckY
    JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
    LDA playerCollides
    CMP TRUE
    BNE .checkSecond
    LDA playerTopLeftX
    CLC
    ADC #$01
    STA playerTopLeftX
    JMP .jumpToEnd                   ; if collision, then dont store new position
.checkSecond:
    LDA playerCollisionLeftMiddleX
    STA collisionPointToCheckX
    LDA playerCollisionLeftMiddleY
    STA collisionPointToCheckY
    JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
    LDA playerCollides
    CMP TRUE
    BNE .checkThird
    LDA playerTopLeftX
    CLC
    ADC #$01
    STA playerTopLeftX
    JMP .jumpToEnd                   ; if collision, then dont store new position
.checkThird:
    LDA playerCollisionBotLeftX
    STA collisionPointToCheckX
    LDA playerCollisionBotLeftY
    STA collisionPointToCheckY
    JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
    LDA playerCollides
    CMP TRUE
    BNE .jumpToEnd
    LDA playerTopLeftX
    CLC
    ADC #$01
    STA playerTopLeftX
    JMP .jumpToEnd                   ; if collision, then dont store new position

.changeRoomLeft:
    LDA gamestate
    CMP #$01
    BEQ .jumpToEnd
    CMP #$04
    BEQ .jumpToEnd
    CMP #$07
    BEQ .jumpToEnd
    SEC
    SBC #$01
    STA gamestate
    LDA #$F0
    STA playerTopLeftX
    LDA #$01
    STA gamestateChange

	LDA #3
    LDX #FT_SFX_CH0     ;entering new room sound
    JSR FamiToneSfxPlay 

.jumpToEnd:
    JMP MovementDone
LeftDone:
 
HandleRight:
    LDA buttons
    AND #%00000001 ; right pressed
    BNE .right
    JMP RightDone
.right:
    LDA #$01
    STA playerDirection
    LDA #$01
    STA playerDirectionLast
.isCounterZero:
    LDA playerACounter
    CMP #$00
    BEQ .isZeroOrOne
    CMP #$01
    BEQ .isZeroOrOne
    SEC
    SBC #$02
    STA playerACounter
.isZeroOrOne:                         ; jump to next screen
    LDA playerTopLeftX
    CMP #$F0
    BEQ .changeRoomRight
    CLC
    ADC #$01
    STA playerTopLeftX
.terrainCollisionRoom01
    JSR UpdateRightCollisionPoints
    LDA playerCollisionTopRightX
    STA collisionPointToCheckX
    LDA playerCollisionTopRightY
    STA collisionPointToCheckY
    JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
    LDA playerCollides
    CMP TRUE
    BNE .checkSecond
    LDA playerTopLeftX
    SEC
    SBC #$01
    STA playerTopLeftX
    JMP .jumpToEnd                   ; if collision, then dont store new position
.checkSecond:
    LDA playerCollisionRightMiddleX
    STA collisionPointToCheckX
    LDA playerCollisionRightMiddleY
    STA collisionPointToCheckY
    JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
    LDA playerCollides
    CMP TRUE
    BNE .checkThird
    LDA playerTopLeftX
    SEC
    SBC #$01
    STA playerTopLeftX
    JMP .jumpToEnd                   ; if collision, then dont store new position
.checkThird:
    LDA playerCollisionBotRightX
    STA collisionPointToCheckX
    LDA playerCollisionBotRightY
    STA collisionPointToCheckY
    JSR BackgroundCollision            ; calculates player collision and stores in "playerCollides"
    LDA playerCollides
    CMP TRUE
    BNE .jumpToEnd
    LDA playerTopLeftX
    SEC
    SBC #$01
    STA playerTopLeftX
    JMP .jumpToEnd                   ; if collision, then dont store new position

.changeRoomRight:
    LDA gamestate
    CMP #$03
    BEQ RightDone
    CMP #$06
    BEQ RightDone
    CMP #$09
    BEQ RightDone
    CLC
    ADC #$01
    STA gamestate
    LDA #$00
    STA playerTopLeftX
    LDA #$01
    STA gamestateChange
    JMP RightDone

	LDA #3
    LDX #FT_SFX_CH0     ;entering new room sound
    JSR FamiToneSfxPlay 

.jumpToEnd:
    JMP MovementDone
RightDone:
MovementDone:

;;;;; Player animation

Animation:
    LDA playerACounter
    CMP #$00
    BEQ .changeAnimation
    SEC
    SBC #$01
    STA playerACounter
    JMP AnimationDone
.changeAnimation:
    LDX playerSecondSpriteCounter
    CPX #$01
    BNE switchOff
    LDX #$00
    STX playerSecondSpriteCounter
    JMP setCounter
switchOff:
    LDX #$01
    STX playerSecondSpriteCounter

	LDA #6
    LDX #FT_SFX_CH0     ;walking sound
    JSR FamiToneSfxPlay 
setCounter:
    LDA #$30
    STA playerACounter

	LDA #6
    LDX #FT_SFX_CH0     ;walking sound
    JSR FamiToneSfxPlay 
AnimationDone:

    JMP GameEngineDone

;;; verschiedene subroutinen

;; controller wird gelesen
ReadController1:
	LDA #$01
	STA $4016
	LDA #$00
	STA $4016
	LDX #$08
ReadController1Loop:
	LDA $4016
	LSR A            ; bit0 -> Carry
	ROL buttons      ; bit0 <- Carry
	DEX
	BNE ReadController1Loop

    RTS

;;;;; function for generating enemy projectiles

GenerateProjectile:
    LDA entityDirection
    CMP #$1B
    BEQ .down
    CMP #$01
    BEQ .right
    CMP #$1D
    BEQ .up
    CMP #$00
    BEQ .left
.down:
    LDX projectileSpriteAddr
    LDA entityTopLeftY
    CLC
    ADC #$10               ; enemy y-pos plus 16 px
    STA $0200, X
    LDA #$10               ; 10 tile
    STA $0201, X
    LDA #%00000011
    STA $0202, X
    LDA entityTopLeftX     ; enemy x-pos
    STA $0203, X
    JMP .projectileGenerateDone
.right:
    LDX projectileSpriteAddr
    LDA entityTopLeftY     ; player y-pos
    CLC
    ADC #$03
    STA $0200, X
    LDA #$10               ; first tile
    STA $0201, X
    LDA #%00000011
    STA $0202, X
    LDA entityTopLeftX     ; player x-pos plus 16 px
    CLC
    ADC #$10
    STA $0203, X
    JMP .projectileGenerateDone
.up:
    LDX projectileSpriteAddr
    LDA entityTopLeftY
    SEC
    SBC #$08               ; enemy y-pos minus 8 px
    STA $0200, X
    LDA #$10               ; first tile
    STA $0201, X
    LDA #%00000011
    STA $0202, X
    LDA entityTopLeftX     ; enemy x-pos plus 8 px
    CLC
    ADC #$08
    STA $0203, X
    JMP .projectileGenerateDone
.left:
    LDX projectileSpriteAddr
    LDA entityTopLeftY     ; enemy y-pos
    CLC
    ADC #$03
    STA $0200, X
    LDA #$10               ; first tile
    STA $0201, X
    LDA #%00000011
    STA $0202, X
    LDA entityTopLeftX     ; enemy x-pos minus 8 px
    SEC
    SBC #$08
    STA $0203, X
.projectileGenerateDone:
    RTS

;;;;; function for collsision with all enemys in the room
   
CheckCollisionEnemys:
    LDA offsetEnemyCollision
    CLC
    ADC #$10
    STA offsetEnemyCollision
    TAX 
    LDA $0261, X                  ; takes tine-number of enemy
    CMP #$00
    BNE .continue
    JMP .checkDone                ; no enemys left
.continue:
    LDA $0260, X                  ; takes y-Pos of enemy
    CMP #$01
    BEQ CheckCollisionEnemys
    STA entityTopLeftY
    INX
    INX
    INX
    LDA $0260, X                  ; takes X-Pos of enemy
    STA entityTopLeftX
.checkEnemyCollisionLoop:
    LDA playerTopLeftX
    CMP entityTopLeftX
    BMI .playerSmallerValueX      ; branch on minus
.playerBiggerValueX:
    LDA playerTopLeftX
    SEC
    SBC entityTopLeftX
    CMP #$0C
    BMI .checkEnemyCollisionY     ; y is colliding, check for x
    JMP CheckCollisionEnemys      ; y is not colliding, check next enemy
.playerSmallerValueX:
    LDA entityTopLeftX
    SEC
    SBC playerTopLeftX
    CMP #$0E
    BMI .checkEnemyCollisionY
    JMP CheckCollisionEnemys
.checkEnemyCollisionY:
    LDA playerTopLeftY
    CMP entityTopLeftY
    BMI .playerSmallerValueY       ; branch on minus
.playerBiggerValueY:
    LDA playerTopLeftY
    SEC
    SBC entityTopLeftY
    CMP #$10
    BMI .collision                 ; x is also colliding, jmp collision
    JMP CheckCollisionEnemys       ; x is not colliding, check next enemy
.playerSmallerValueY:
    LDA entityTopLeftY
    SEC
    SBC playerTopLeftY
    CMP #$10
    BMI .collision
    JMP CheckCollisionEnemys
.collision:
    LDA lifeLossCooldown
    CMP #$00
    BNE .checkDone
    LDA playerLifeCount
    SEC
    SBC #$01
    STA playerLifeCount
    CMP #$00
    BNE .noGameOver                 ; falls Leben auf 0 fallen, wird für raumwechsel (in game over screen) vorbereitet
    LDA #$01
    STA gamestateChange
.noGameOver
    LDA #$30
    STA lifeLossCooldown
.checkDone:
    LDA #$00
    STA offsetEnemyCollision
    RTS

;;;;; main function that handles all projectiles

MainProjectileFunction:
.playerProj0:
    LDA playerProj0                   ; check for projectile
    CMP #$01
    BNE .playerProj0Done
    LDA $0260                         ; variables for player
    STA projectileTopLeftY            ; projectile 0 gets set
    LDA $0263
    STA projectileTopLeftX
    LDA playerProjectile0Direction
    STA projectileDirection
    LDA #$60
    STA projectileSpriteAddr
    JSR UpdateProjectile
    JSR CheckCollisionPlayerProjectile
    LDA collisionHappened             ; clear variables, 
    CMP #$01                          ; if collision happene
    BNE .playerProj0Done
    LDA #$00
    STA playerProj0
    STA collisionHappened
.playerProj0Done:
.playerProj1:
    LDA playerProj1
    CMP #$01
    BNE .playerProj1Done
    LDA $0264                         ; variables for player projectile 0 gets set
    STA projectileTopLeftY
    LDA $0267
    STA projectileTopLeftX
    LDA playerProjectile1Direction
    STA projectileDirection
    LDA #$64
    STA projectileSpriteAddr
    JSR UpdateProjectile
    JSR CheckCollisionPlayerProjectile
    LDA collisionHappened
    CMP #$01
    BNE .playerProj1Done
    LDA #$00
    STA playerProj1
    STA collisionHappened
.playerProj1Done:
.enemy0Proj0:
    LDA enemy0Proj0
    CMP #$01
    BNE .enemy0Proj0Done
    LDA $0240                         ; variables for enemy projectile 0.0 gets set
    STA projectileTopLeftY    
    LDA $0243
    STA projectileTopLeftX
    LDA enemy0Projectile0Direction
    STA projectileDirection
    LDA #$40
    STA projectileSpriteAddr
    JSR UpdateProjectile
    JSR CheckCollisionEnemyProjectile
    LDA collisionHappened
    CMP #$01
    BNE .enemy0Proj0Done
    LDA #$00
    STA enemy0Proj0
    STA collisionHappened
.enemy0Proj0Done:
.enemy1Proj0:
    LDA enemy1Proj0
    CMP #$01
    BNE .enemy1Proj0Done
    LDA $0244                         ; variables for enemy projectile 1.0 gets set
    STA projectileTopLeftY
    LDA $0247
    STA projectileTopLeftX
    LDA enemy1Projectile0Direction
    STA projectileDirection
    LDA #$44
    STA projectileSpriteAddr
    JSR UpdateProjectile
    JSR CheckCollisionEnemyProjectile
    LDA collisionHappened
    CMP #$01
    BNE .enemy1Proj0Done
    LDA #$00
    STA enemy1Proj0
    STA collisionHappened
.enemy1Proj0Done:
.enemy1Proj1:
    LDA enemy1Proj1
    CMP #$01
    BNE .enemy1Proj1Done
    LDA $0248                         ; variables for enemy projectile 1.1 gets set
    STA projectileTopLeftY
    LDA $024B
    STA projectileTopLeftX
    LDA enemy1Projectile1Direction
    STA projectileDirection
    LDA #$48
    STA projectileSpriteAddr
    JSR UpdateProjectile
    JSR CheckCollisionEnemyProjectile
    LDA collisionHappened
    CMP #$01
    BNE .enemy1Proj1Done
    LDA #$00
    STA enemy1Proj1
    STA collisionHappened
.enemy1Proj1Done:
.enemy2Proj0:
    LDA enemy2Proj0
    CMP #$01
    BNE .enemy2Proj0Done
    LDA $024C                         ; variables for enemy projectile 2.0 gets set
    STA projectileTopLeftY
    LDA $024F
    STA projectileTopLeftX
    LDA enemy2Projectile0Direction
    STA projectileDirection
    LDA #$4C
    STA projectileSpriteAddr
    JSR UpdateProjectile
    JSR CheckCollisionEnemyProjectile
    LDA collisionHappened
    CMP #$01
    BNE .enemy2Proj0Done
    LDA #$00
    STA enemy2Proj0
    STA collisionHappened
.enemy2Proj0Done:
    RTS

;;;;; function for collsision with enemy projectile

CheckCollisionEnemyProjectile:
    LDA playerTopLeftX
    CMP projectileTopLeftX
    BMI .playerSmallerValueX      ; branch on minus
.playerBiggerValueX:
    LDA playerTopLeftX
    SEC
    SBC projectileTopLeftX
    CMP #$08
    BMI .checkEnemyCollisionY     ; y is colliding, check for x
    JMP CheckCollisionEnemys      ; y is not colliding, check next enemy
.playerSmallerValueX:
    LDA projectileTopLeftX
    SEC
    SBC playerTopLeftX
    CMP #$10
    BMI .checkEnemyCollisionY
    JMP CheckCollisionEnemys
.checkEnemyCollisionY:
    LDA playerTopLeftY
    CMP projectileTopLeftY
    BMI .playerSmallerValueY       ; branch on minus
.playerBiggerValueY:
    LDA playerTopLeftY
    SEC
    SBC projectileTopLeftY
    CMP #$08
    BMI .collision                 ; x is also colliding, jmp collision
    JMP CheckCollisionEnemys       ; x is not colliding, check next enemy
.playerSmallerValueY:
    LDA projectileTopLeftY
    SEC
    SBC playerTopLeftY
    CMP #$11
    BMI .collision
    JMP CheckCollisionEnemys
.collision:
    JSR ClearProjectile
    LDA lifeLossCooldown
    CMP #$00
    BNE .checkDone
    LDA playerLifeCount
    SEC
    SBC #$01
    STA playerLifeCount
    CMP #$00
    BNE .noGameOver                ; falls Leben auf 0 fallen, wird für raumwechsel (in game over screen) vorbereitet
    LDA #$01
    STA gamestateChange
.noGameOver
    LDA #$30
    STA lifeLossCooldown
.checkDone:
    RTS

;;;;; check collision of player projectiles with enemys

CheckCollisionPlayerProjectile:
    LDA offsetEnemyProjectileCollision
    CLC
    ADC #$10
    STA offsetEnemyProjectileCollision
    LDX offsetEnemyProjectileCollision
    LDA $0261, X                      ; takes tile number of enemy; if $00 no enemy left
    CMP #$00
    BEQ .checkDone
    LDA $0260, X                      ; takes Y-Pos of enemy; if $01 enemy dead, check next
    CMP #$01
    BEQ CheckCollisionPlayerProjectile
    STA entityTopLeftY
    INX
    INX
    INX
    LDA $0260, X                      ; takes X-Pos of enemy
    STA entityTopLeftX
.checkEnemyProjCollisionLoop:
    LDA projectileTopLeftX
    CMP entityTopLeftX
    BMI .projectileSmallerValueX      ; branch on minus
.projectileBiggerValueX:
    LDA projectileTopLeftX
    SEC
    SBC entityTopLeftX
    CMP #$10
    BMI .checkEnemyProjCollisionY     ; y is colliding, check for x
    JMP CheckCollisionPlayerProjectile      ; y is not colliding, check next enemy
.projectileSmallerValueX:
    LDA entityTopLeftX
    SEC
    SBC projectileTopLeftX
    CMP #$8
    BMI .checkEnemyProjCollisionY
    JMP CheckCollisionPlayerProjectile
.checkEnemyProjCollisionY:
    LDA projectileTopLeftY
    CMP entityTopLeftY
    BMI .projectileSmallerValueY      ; branch on minus
.projectileBiggerValueY:
    LDA projectileTopLeftY
    SEC
    SBC entityTopLeftY
    CMP #$10
    BMI .collision                    ; x is also colliding, jmp collision
    JMP CheckCollisionPlayerProjectile      ; x is not colliding, check next enemy
.projectileSmallerValueY:
    LDA entityTopLeftY
    SEC
    SBC projectileTopLeftY
    CMP #$05
    BMI .collision
    JMP CheckCollisionPlayerProjectile
.collision:
    LDX offsetEnemyProjectileCollision
    LDY #$00
    .loop:
    LDA deactivateSprite, Y                ; enemy gets (01,FF,00,00) 
    STA $0260, X
    INX
    INY
    CPY #$10                          ; 4 sprites
    BNE .loop
    JSR ClearProjectile               ; projectile gets cleared after hit with enemy
    JMP CheckCollisionPlayerProjectile
.checkDone:
    LDA #$00
    STA offsetEnemyProjectileCollision
    RTS

;;;;; updates the projectile 

UpdateProjectile:
    LDA projectileDirection
    CMP #$1D
    BEQ .moveUp
    CMP #$1B
    BEQ .moveDown
    CMP #$01
    BEQ .moveRight
    CMP #$00
    BEQ .moveLeft
.moveUp:
    LDY projectileSpriteAddr
    LDX $0200, Y
    CPX #$08
    BEQ .jmpClearProjectile
    DEX
    TXA
    LDY projectileSpriteAddr
    STA $0200, Y
    JMP .updateProjectileDone
.moveDown:
    LDY projectileSpriteAddr
    LDX $0200, Y
    CPX #$FF
    BEQ .jmpClearProjectile
    INX
    TXA
    LDY projectileSpriteAddr
    STA $0200, Y
    JMP .updateProjectileDone
.moveLeft:
    LDY projectileSpriteAddr
    LDX $0203, Y
    CPX #$00
    BEQ .jmpClearProjectile
    CPX #$F8
    BEQ .jmpClearProjectile
    DEX
    TXA
    LDY projectileSpriteAddr
    STA $0203, Y
    JMP .updateProjectileDone
.moveRight:
    LDY projectileSpriteAddr
    LDX $0203, Y
    CPX #$F8
    BEQ .jmpClearProjectile
    CPX #$00
    BEQ .jmpClearProjectile
    INX
    TXA
    LDY projectileSpriteAddr
    STA $0203, Y
.updateProjectileDone:
    JMP ProjTerrainCollision
.jmpClearProjectile
    JMP ClearProjectile

;;;; checks projectile collsision with walls, using player collision function (variables are reused)

ProjTerrainCollision:
    LDA playerTopLeftX                ; storing projectile pos temporarily in player pos variables
    STA bufferPlayerTopLeftX          ; collsision funktion uses them
    LDA playerTopLeftY
    STA bufferPlayerTopLeftY       
    LDA projectileTopLeftX     ; hitbox of projectile gets a bit optimised 
    SEC
    SBC #$06
    STA playerTopLeftX
    LDA projectileTopLeftY
    SEC
    SBC #$02
    STA playerTopLeftY
    JSR UpdateRightCollisionPoints    ; funktion for collision detection
    LDA playerCollisionTopRightX
    STA collisionPointToCheckX
    LDA playerCollisionTopRightY
    STA collisionPointToCheckY
    JSR BackgroundCollision
    LDA playerCollides
    CMP TRUE
    BNE .checkSecond
    JMP .jumpToClean
.checkSecond:
    LDA playerCollisionRightMiddleX
    STA collisionPointToCheckX
    LDA playerCollisionRightMiddleY
    STA collisionPointToCheckY
    JSR BackgroundCollision
    LDA playerCollides
    CMP TRUE
    BNE .checkThird
    JMP .jumpToClean
.checkThird:
    LDA playerCollisionBotRightX
    STA collisionPointToCheckX
    LDA playerCollisionBotRightY
    STA collisionPointToCheckY
    JSR BackgroundCollision
    LDA playerCollides
    CMP TRUE
    BNE .end
.jumpToClean:
    JSR ClearProjectile
.end:
    LDA bufferPlayerTopLeftY       ; player pos gets set right
    STA playerTopLeftY
    LDA bufferPlayerTopLeftX
    STA playerTopLeftX
    RTS

;;;;;

ClearProjectile:
    LDA #$01
    STA collisionHappened
    LDY #$00
    LDX projectileSpriteAddr
    .loop:
    LDA deactivateSprite, Y
    STA $0200, X
    INX
    INY
    CPY #$04   ;1 sprite
    BNE .loop
    RTS

;;;;; updates the player sprites

UpdateSprites:
    LDA playerDirection
    CMP #$FF
    BEQ .jumpToAttackSprite
    CMP #$01
    BEQ .lookRight
    LDA playerSecondSpriteCounter
    CMP #$01
    BEQ .secondSprite
    LDA #$03
    CLC
    ADC playerDirection
    STA $0201
    LDA #$04
    CLC
    ADC playerDirection
    STA $0205
    LDA #$13
    CLC
    ADC playerDirection
    STA $0209
    LDA #$14
    CLC
    ADC playerDirection
    STA $020D
    JMP .attributesOther
.jumpToAttackSprite:
    JMP AttackSprite
.secondSprite:
    LDA #$01
    CLC
    ADC playerDirection
    STA $0201
    LDA #$02
    CLC
    ADC playerDirection
    STA $0205
    LDA #$11
    CLC
    ADC playerDirection
    STA $0209
    LDA #$12
    CLC
    ADC playerDirection
    STA $020D
    JMP .attributesOther
.lookRight:
    LDA playerSecondSpriteCounter
    CMP #$01
    BEQ .secondSpriteRight
    LDA #$08
    STA $0201
    LDA #$07
    STA $0205
    LDA #$18
    STA $0209
    LDA #$17
    STA $020D
    JMP .attributesRight
.secondSpriteRight:
    LDA #$0A
    STA $0201
    LDA #$09
    STA $0205
    LDA #$1A
    STA $0209
    LDA #$19
    STA $020D
.attributesRight:
    LDA #%01000000
    STA $0202
    STA $0206
    STA $020A
    STA $020E
    JMP ChangePosition
.attributesOther:
    LDA #%00000000
    STA $0202
    STA $0206
    STA $020A
    STA $020E
ChangePosition:
    LDA playerTopLeftY
    STA $0200 ;sprite1
    STA $0204 ; sprite2 rechts oben
    CLC
    ADC #$08
    STA $0208 ; sprite links unten
    STA $020C ; sprite rechts unten

    LDA playerTopLeftX
    STA $0203 ;sprite1
    STA $020B ; sprite3 links unten
    CLC
    ADC #$08
    STA $0207 ; sprite rechts oben
    STA $020F ; sprite rechts unten
    LDA #$00
    STA playerDirection
    RTS

;;;;; updates the players attack sprites

AttackSprite:
    LDA playerDirectionLast
    CMP #$01
    BEQ .attackRight
    LDA #$05           ; tile of the left attack
    CLC
    ADC playerDirectionLast
    STA $0201
    LDA #$06
    CLC
    ADC playerDirectionLast
    STA $0205
    LDA #$15
    CLC
    ADC playerDirectionLast
    STA $0209
    LDA #$16
    CLC
    ADC playerDirectionLast
    STA $020D
    LDA #%00000000     ; normal attributes
    STA $0202
    STA $0206
    STA $020A
    STA $020E
    JMP .attackSpriteDone
.attackRight:          ; uses the left one but flips it
    LDA #$06
    STA $0201
    LDA #$05
    STA $0205
    LDA #$16
    STA $0209
    LDA #$15
    STA $020D
    LDA #%01000000
    STA $0202
    STA $0206
    STA $020A
    STA $020E
.attackSpriteDone:

AttackProjectile:
    LDA playerProjectileCooldown ; if cooldown at 00 shoot projectile else skip function
    CMP #$00
    BEQ .continue
    JMP .attackProjectileDone
    .continue:                   ; set cooldown to 20 
    CLC
    ADC #$20
    STA playerProjectileCooldown
.proj0:                      ; take proj0 slot if free or all full unless currently overwritten
    LDA overwritePlayerProjectile
    CMP #$01
    BEQ .proj0Write
    CMP #$02
    BEQ .proj1Write
    LDA playerProj0
    CMP #$00
    BNE .proj1
    .proj0Write:
    LDA #$02
    STA overwritePlayerProjectile
    LDA #$01
    STA playerProj0
    LDA #$60
    STA projectileSpriteAddr
    LDA playerDirectionLast
    STA playerProjectile0Direction
    JMP .start
.proj1:                      ; take proj1 slot if free or all full and slot0 currently overwritten
    LDA playerProj1
    CMP #$00
    BNE .overwrite0
    .proj1Write:
    LDA #$00
    STA overwritePlayerProjectile     ; if both slots overwritten start with overwriting slot 0
    LDA #$01
    STA playerProj1
    LDA #$64
    STA projectileSpriteAddr
    LDA playerDirectionLast
    STA playerProjectile1Direction
    JMP .start
.overwrite0:                   ; overwrite slot 0 if all full
    LDA #$01
    STA overwritePlayerProjectile
    JMP .proj0
    .start:
    LDA playerDirectionLast
    STA entityDirection
    LDA playerTopLeftX
    STA entityTopLeftX
    LDA playerTopLeftY
    STA entityTopLeftY
    JSR GenerateProjectile
.attackProjectileDone:
    LDA #$00
    STA playerDirection
    RTS

;;;;; checks if the lifecooldown is at 00, if true player can loose a life

HandleLifeCount:
    LDA playerLifeCount
    CMP #$00
    BNE .continue
    JMP GameOverScreen
    .continue:
    LDA lifeLossCooldown
    CMP #$00
    BEQ .noCooldown
    SEC
    SBC #$01
    STA lifeLossCooldown
.noCooldown:
    JMP UpdatePlayerLifes

;;;;; displays the lifebar

UpdatePlayerLifes:
    LDA playerTopLeftY               ; load player x, y and move health bar accordingly
    SEC
    SBC #$08
    STA $0210
    STA $0214
    STA $0218
    LDA playerTopLeftX
    SEC
    SBC #$05
    STA $0213
    CLC
    ADC #$09
    STA $0217
    CLC
    ADC #$09
    STA $021B
    LDA #%00000011
    STA $0212
    STA $0216
    STA $021A
    LDA playerLifeCount            ; count players lifes
    CMP #$03                      ; if 3 hearts left, display all three
    BNE .twoHearts
    LDA #$0F
    STA $0211
    STA $0215
    STA $0219
    JMP .attributes
.twoHearts:                    ; two hearts left
    CMP #$02
    BNE .oneHeart
    LDA #$0F
    STA $0211
    STA $0215
    LDA #$1F
    STA $0219
    JMP .attributes
.oneHeart:                     ; one heart left
    LDA #$0F
    STA $0211
    LDA #$1F
    STA $0215
    STA $0219
.attributes:
    LDA #%00000011
    STA $0212
    STA $0216
    STA $021A
    RTS

;;;;;;; cleares sprites of the old room 

ClearSprites:
    LDA gamestateChange
    CMP #$00
    BEQ .clearSpritesDone
    LDX #$00
.loadClearSpritesLoop:
    LDA #$00
    STA $0240, x
    INX
    CPX #$60                                    ; clears only 24 sprites for now
    BNE .loadClearSpritesLoop
    LDA #$00
    STA gamestateChange
    STA roomInitialized
.clearSpritesDone:
    RTS

;;;;;;;; Checks Background-Collision

BackgroundCollision:
    LDA collisionPointToCheckX                   ;last 8 digits of x value
    LSR A
    LSR A
    LSR A
    AND #%00000111
    TAX
    LDA collisionPointToCheckY                   ; used for offset: player Y position /8 *4 (has to be processed in this order)
    LSR A
    LSR A
    LSR A
    ASL A
    ASL A
    STA playerInRow
    LDA collisionPointToCheckX                   ; used for offset: player X position /64
    LSR A
    LSR A
    LSR A
    LSR A
    LSR A
    LSR A
    CLC
    ADC playerInRow
    TAY
    LDA gamestate
    CMP #Room01
    BEQ .r01
    CMP #Room02
    BEQ .r02
    JMP .r03
.r01:
    LDA room01CollisionMatrix, y
    JMP .collisionLoop
.r02
    LDA room02CollisionMatrix, y
    JMP .collisionLoop
.r03
    LDA room01CollisionMatrix, y
.collisionLoop:
    CPX #$00
    BEQ .getValue                       ; this loop compares the last 8 digits of player X position
    ASL A                               ; to zero: if it is, the leftmostbit of the chosen byte of
    DEX                                 ; room01CollisionMatrix will be looked at for collision/no collisio (bit = 1/0)
    JMP .collisionLoop                  ; if player X position is not 0, it will be decreased by 1 and the Matrix-Byte
.getValue:                              ; will be rotated left once, making the loop check the secont bit of that byte and so on
    AND #%10000000
    BNE .dontMove
    LDA FALSE
    STA playerCollides
    RTS
.dontMove:
    LDA TRUE
    STA playerCollides
    RTS

;;;;;;;;;;;;;;;; updated Punkte, die für die Collision benötigt werden

UpdateTopCollisionPoints:
    LDA playerTopLeftX               ; lade topLeftX und verteile auf die zu Prüfenden Punkte
    CLC
    ADC #$05
    STA playerCollisionTopLeftX
    ADC #$05
    STA playerCollisionTopRightX
    LDA playerTopLeftY
    STA playerCollisionTopLeftY
    STA playerCollisionTopRightY
    RTS

UpdateBotCollisionPoints:
    LDA playerTopLeftX               ; lade topLeftX und verteile auf die zu Prüfenden Punkte
    CLC
    ADC #$05
    STA playerCollisionBotLeftX
    ADC #$05
    STA playerCollisionBotRightX
    LDA playerTopLeftY
    CLC
    ADC #$0F
    STA playerCollisionBotLeftY
    STA playerCollisionBotRightY
    RTS

UpdateLeftCollisionPoints:
    LDA playerTopLeftX               ; lade topLeftX und verteile auf die zu Prüfenden Punkte
    CLC
    ADC #$05
    STA playerCollisionTopLeftX
    STA playerCollisionLeftMiddleX
    STA playerCollisionBotLeftX
    LDA playerTopLeftY
    STA playerCollisionTopLeftY
    CLC
    ADC #$07
    STA playerCollisionLeftMiddleY
    CLC
    ADC #$08
    STA playerCollisionBotLeftY
    RTS

UpdateRightCollisionPoints:
    LDA playerTopLeftX               ; lade topLeftX und verteile auf die zu Prüfenden Punkte
    CLC
    ADC #$0A
    STA playerCollisionTopRightX
    STA playerCollisionRightMiddleX
    STA playerCollisionBotRightX
    LDA playerTopLeftY
    STA playerCollisionTopRightY
    CLC
    ADC #$07
    STA playerCollisionRightMiddleY
    CLC
    ADC #$08
    STA playerCollisionBotRightY
    RTS

;;;;;;; Daten etc

  .bank 1
  .org $E000

playerInit:
  .db $08,$01,$00,$00  ; erste zeile wird abgeschnitten, also wird bei y=8 gestartet
  .db $08,$02,$00,$08
  .db $10,$11,$00,$00
  .db $10,$12,$00,$08

enemy0Init:
  .db $98,$24,$01,$C8
  .db $98,$25,$01,$D0
  .db $A0,$34,$01,$C8  
  .db $A0,$35,$01,$D0

enemy1Init:
  .db $20,$24,$01,$28
  .db $20,$25,$01,$30
  .db $28,$34,$01,$28  
  .db $28,$35,$01,$30

enemy2Init:
  .db $58,$26,$02,$50
  .db $58,$27,$02,$58
  .db $60,$36,$02,$50  
  .db $60,$37,$02,$58

deactivateSprite:
  .db $01,$FF,$00,$00
  .db $01,$FF,$00,$00
  .db $01,$FF,$00,$00
  .db $01,$FF,$00,$00
      
palette:
  .db $0F,$3D,$09,$2D,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$0F  ; background palette
  .db $0F,$18,$0D,$2D,$31,$13,$18,$0D,$0F,$1D,$30,$24,$31,$16,$06,$27  ; sprite palette

attribute:
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000 ;;immer Palette 1 benutzen
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

LoadTopBotWall:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadTopBotWall ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$01,$11,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$01,$11,$FF ; default Column 1
.column12:
  .db $FF,$02,$12,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$02,$12,$FF ; default Column 2

LoadDoorLeftRight:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadDoorLeftRight ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$01,$11,$01,$11,$01,$11,$01,$11,$01,$11,$01,$11,$00,$10,$00,$10,$01,$11,$01,$11,$01,$11,$01,$11,$01,$11,$01,$11,$FF ; doorVertical Column 1
.column12:
  .db $FF,$02,$12,$02,$12,$02,$12,$02,$12,$02,$12,$02,$12,$03,$13,$03,$13,$02,$12,$02,$12,$02,$12,$02,$12,$02,$12,$02,$12,$FF ; doorVertical Column 2

LoadDoorBot:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadDoorBot ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$01,$11,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$FF ; doorVertical Column 1
.column12:
  .db $FF,$02,$12,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$FF ; doorVertical Column 2

LoadBigPillarLeftLeft:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadBigPillarLeftLeft ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$01,$11,$00,$10,$00,$10,$00,$10,$00,$10,$04,$06,$06,$06,$06,$06,$06,$14,$00,$10,$00,$10,$00,$10,$00,$10,$01,$11,$FF ; doorVertical Column 1
.column12:
  .db $FF,$02,$12,$03,$13,$03,$13,$03,$13,$03,$13,$06,$04,$06,$06,$06,$06,$14,$16,$03,$13,$03,$13,$03,$13,$03,$13,$02,$12,$FF ; doorVertical Column 2

LoadBigPillarLeftMiddle:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadBigPillarLeftMiddle ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$01,$11,$00,$10,$00,$10,$00,$10,$00,$10,$06,$06,$04,$06,$06,$14,$16,$16,$00,$10,$00,$10,$00,$10,$00,$10,$01,$11,$FF ; doorVertical Column 1
.column12:
  .db $FF,$02,$12,$03,$13,$03,$13,$03,$13,$03,$13,$06,$06,$06,$07,$07,$16,$16,$16,$03,$13,$03,$13,$03,$13,$03,$13,$02,$12,$FF ; doorVertical Column 2

LoadBigPillarRightMiddle:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadBigPillarRightMiddle ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$01,$11,$00,$10,$00,$10,$00,$10,$00,$10,$06,$06,$06,$07,$07,$16,$16,$16,$00,$10,$00,$10,$00,$10,$00,$10,$01,$11,$FF ; doorVertical Column 1
.column12:
  .db $FF,$02,$12,$03,$13,$03,$13,$03,$13,$03,$13,$06,$06,$05,$16,$16,$15,$16,$16,$03,$13,$03,$13,$03,$13,$03,$13,$02,$12,$FF ; doorVertical Column 2

LoadBigPillarRightRight:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadBigPillarRightRight ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$01,$11,$00,$10,$00,$10,$00,$10,$00,$10,$06,$05,$16,$16,$16,$16,$15,$16,$00,$10,$00,$10,$00,$10,$00,$10,$01,$11,$FF ; doorVertical Column 1
.column12:
  .db $FF,$02,$12,$03,$13,$03,$13,$03,$13,$03,$13,$05,$16,$16,$16,$16,$16,$16,$15,$03,$13,$03,$13,$03,$13,$03,$13,$02,$12,$FF ; doorVertical Column 2

LoadFloorOnly:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadFloorOnly ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$00,$10,$FF ; doorVertical Column 1
.column12:
  .db $FF,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$03,$13,$FF ; doorVertical Column 2


LoadPillars1:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadPillars1 ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$01,$11,$00,$10,$00,$10,$00,$10,$01,$11,$00,$10,$00,$10,$00,$10,$00,$10,$01,$11,$00,$10,$00,$10,$00,$10,$01,$11,$FF ; default Column 1
.column12:
  .db $FF,$02,$12,$03,$13,$03,$13,$03,$13,$02,$12,$03,$13,$03,$13,$03,$13,$03,$13,$02,$12,$03,$13,$03,$13,$03,$13,$02,$12,$FF ; default Column 2

LoadBlack1:
	LDA .column11, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE LoadBlack1 ; wiederholen, falls nicht 30 teile geladen
	INC Column
	LDA #$20
    STA PPUADDR
    LDA #$00
    CLC
    ADC Column
    STA PPUADDR
    LDX #$00
.loop2:
    LDA .column12, x
	STA PPUDATA             ; write to PPU
	INX                     ; Y = Y + 1
    CPX #$1E                ; copy 30 tiles
	BNE .loop2  ; wiederholen, falls nicht 30 teile geladen
	RTS
.column11:
  .db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; black Column 1
.column12:
  .db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF ; still black


room01CollisionMatrix:
  .db %11111111,%11111111,%11111111,%11111111
  .db %11111111,%11111100,%00111111,%11111111
  .db %11111111,%11111100,%00111111,%11111111
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%11000000,%00000011,%00000011
  .db %11000000,%11000000,%00000011,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %00000000,%00000000,%00000000,%00000000
  .db %00000000,%00000000,%00000000,%00000000
  .db %00000000,%00000000,%00000000,%00000000
  .db %00000000,%00000000,%00000000,%00000000
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%11000000,%00000011,%00000011
  .db %11000000,%11000000,%00000011,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11111111,%11111100,%00111111,%11111111
  .db %11111111,%11111100,%00111111,%11111111
  .db %11111111,%11111111,%11111111,%11111111

room02CollisionMatrix:
  .db %11111111,%11111111,%11111111,%11111111
  .db %11111111,%11111111,%11111111,%11111111
  .db %11111111,%11111111,%11111111,%11111111
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000011,%11111100,%00111111,%11000011
  .db %11000011,%11111100,%00111111,%11000011
  .db %00000011,%11111100,%00111111,%11000000
  .db %00000011,%11111100,%00111111,%11000000
  .db %00000011,%11111100,%00111111,%11000000
  .db %00000011,%11111100,%00111111,%11000000
  .db %11000011,%11111100,%00111111,%11000011
  .db %11000011,%11111100,%00111111,%11000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11000000,%00000000,%00000000,%00000011
  .db %11111111,%11111100,%00111111,%11111111
  .db %11111111,%11111100,%00111111,%11111111
  .db %11111111,%11111111,%11111111,%11111111

gameOverScreen:
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$80,$81,$81,$81,$81,$81,$81,$81,$81 ; black
  .db $81,$81,$81,$81,$81,$81,$81,$82,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$90,$80,$81,$81,$81,$81,$81,$81,$81 ; black
  .db $81,$81,$81,$81,$81,$81,$82,$92,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$90,$90,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$92,$92,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$90,$90,$20,$20,$47,$41,$4D,$45,$20 ; Game Over
  .db $4F,$56,$45,$52,$20,$20,$92,$92,$20,$20,$20,$20,$20,$20,$20,$20 ; Zeile 10

  .db $20,$20,$20,$20,$20,$20,$20,$90,$90,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$92,$92,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$90,$90,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$92,$92,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$90,$90,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$92,$92,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$90,$90,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$92,$92,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$90,$90,$20,$50,$52,$45,$53,$53,$20 ; PRESS START
  .db $53,$54,$41,$52,$54,$20,$92,$92,$20,$20,$20,$20,$20,$20,$20,$20 ; line 16

  .db $20,$20,$20,$20,$20,$20,$20,$90,$90,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$92,$92,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$90,$A0,$A1,$A1,$A1,$A1,$A1,$A1,$A1 ; black
  .db $A1,$A1,$A1,$A1,$A1,$A1,$A2,$92,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$A0,$A1,$A1,$A1,$A1,$A1,$A1,$A1,$A1 ; black
  .db $A1,$A1,$A1,$A1,$A1,$A1,$A1,$A2,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; line 20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ;black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; black
  .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20 ; line 30

  .org $FFFA     ;first of the three vectors starts here
  .dw NMI        ;when an NMI happens (once per frame if enabled) the
                   ;processor will jump to the label NMI:
  .dw RESET      ;when the processor first turns on or is reset, it will jump
                   ;to the label RESET:
  .dw 0          ;external interrupt IRQ is not used in this tutorial

;;;;;;;;;;; unsere chr rom

  .bank 2
  .org $0000
  .incbin "dungeonXrunner.chr"   ;includes our CHR-Rom
