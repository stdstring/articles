USE master
GO

IF DB_ID (N'TicTacToe') IS NOT NULL
    DROP DATABASE TicTacToe
GO

CREATE DATABASE TicTacToe
GO

USE TicTacToe
GO

IF SCHEMA_ID(N'Core') IS NOT NULL
    DROP SCHEMA Core
GO

CREATE SCHEMA Core
GO

IF SCHEMA_ID(N'CompStrategy') IS NOT NULL
    DROP SCHEMA CompStrategy
GO

CREATE SCHEMA CompStrategy
GO

IF OBJECT_ID(N'Core.GameSession', N'U') IS NOT NULL
    DROP TABLE Core.GameSession
GO

CREATE TABLE Core.GameSession
(
GameID varchar(40) NOT NULL PRIMARY KEY,
/* 'U' = User, 'C' = Comp */
FirstPlayer char(1) NOT NULL CHECK (FirstPlayer = 'U' OR FirstPlayer = 'C'),
NextCompStepGenerator varchar(50) NOT NULL,
/* 'U' = User, 'C' = Comp, 'D' = Draw, NULL = game is not finished */
GameResult char(1) NULL CHECK (GameResult = 'U' OR GameResult = 'C' OR GameResult = 'D')
)
GO

IF OBJECT_ID(N'Core.GameSessionLog', N'U') IS NOT NULL
    DROP TABLE Core.GameSessionLog
GO

CREATE TABLE Core.GameSessionLog
(
GameID varchar(40) NOT NULL,
Number int NOT NULL CHECK((Number >= 1) AND (Number <= 9)),
Step int NOT NULL CHECK(Step IN (11, 12, 13, 21, 22, 23, 31, 32, 33)),
Value char(1) NOT NULL CHECK (Value = 'X' OR Value = 'O'),
CONSTRAINT GameSessionLog_PK PRIMARY KEY (GameID, Number),
CONSTRAINT GameSession_FK FOREIGN KEY (GameID) REFERENCES Core.GameSession(GameID) ON UPDATE CASCADE ON DELETE CASCADE
)
GO

/* INTERNAL */
IF OBJECT_ID (N'Core.GetCellValue', N'FN') IS NOT NULL
    DROP FUNCTION Core.GetCellValue
GO

CREATE FUNCTION Core.GetCellValue (@GameID varchar(40), @Step int)
RETURNS char(1)
AS
BEGIN
    DECLARE @Value char(1)
    SELECT @Value = Value FROM Core.GameSessionLog WHERE (GameId = @GameID) AND (Step = @Step)
    RETURN ISNULL(@Value, ' ')
END
GO

IF OBJECT_ID(N'Core.MakeStep', N'P') IS NOT NULL
    DROP PROCEDURE Core.MakeStep
GO

CREATE PROCEDURE Core.MakeStep @GameID varchar(40), @Step int, @Player char(1)
AS
    SET NOCOUNT ON
    DECLARE @FirstPlayer char(1)
    SELECT @FirstPlayer = FirstPlayer FROM Core.GameSession WHERE GameID = @GameID
    DECLARE @StepCount int
    SELECT @StepCount = COUNT(Number) FROM Core.GameSessionLog WHERE GameID = @GameID
    INSERT Core.GameSessionLog(GameID, Number, Step, Value) VALUES(@GameID, @StepCount + 1, @Step, IIF(@Player = @FirstPlayer, 'X', 'O'))
GO

IF OBJECT_ID (N'Core.GetRowValue', N'FN') IS NOT NULL
    DROP FUNCTION Core.GetRowValue
GO

CREATE FUNCTION Core.GetRowValue (@GameID varchar(40), @Row int)
RETURNS char(3)
AS
BEGIN
    RETURN Core.GetCellValue(@GameID, 10 * @Row + 1) + Core.GetCellValue(@GameID, 10 * @Row + 2) + Core.GetCellValue(@GameID, 10 * @Row + 3)
END
GO

IF OBJECT_ID (N'Core.GetColumnValue', N'FN') IS NOT NULL
    DROP FUNCTION Core.GetColumnValue
GO

CREATE FUNCTION Core.GetColumnValue (@GameID varchar(40), @Column int)
RETURNS char(3)
AS
BEGIN
    RETURN Core.GetCellValue(@GameID, 10 + @Column) + Core.GetCellValue(@GameID, 20 + @Column) + Core.GetCellValue(@GameID, 30 + @Column)
END
GO

IF OBJECT_ID (N'Core.GetDirectDiagonalValue', N'FN') IS NOT NULL
    DROP FUNCTION Core.GetDirectDiagonalValue
GO

CREATE FUNCTION Core.GetDirectDiagonalValue (@GameID varchar(40))
RETURNS char(3)
AS
BEGIN
    RETURN Core.GetCellValue(@GameID, 11) + Core.GetCellValue(@GameID, 22) + Core.GetCellValue(@GameID, 33)
END
GO

IF OBJECT_ID (N'Core.GetInverseDiagonalValue', N'FN') IS NOT NULL
    DROP FUNCTION Core.GetInverseDiagonalValue
GO

CREATE FUNCTION Core.GetInverseDiagonalValue (@GameID varchar(40))
RETURNS char(3)
AS
BEGIN
    RETURN Core.GetCellValue(@GameID, 13) + Core.GetCellValue(@GameID, 22) + Core.GetCellValue(@GameID, 31)
END
GO

IF OBJECT_ID(N'Core.ProcessUserStep', N'P') IS NOT NULL
    DROP PROCEDURE Core.ProcessUserStep
GO

CREATE PROCEDURE Core.ProcessUserStep @GameID varchar(40), @Step int
AS
    SET NOCOUNT ON
    DECLARE @CellValue char(1)
    SELECT @CellValue = Value FROM Core.GameSessionLog WHERE (GameId = @GameID) AND (Step = @Step)
    IF (@CellValue IS NOT NULL)
    BEGIN
        RAISERROR (N'Bad cell', 16, 1)
        RETURN
    END
    EXECUTE Core.MakeStep @GameID, @Step, 'U'
GO

IF OBJECT_ID(N'Core.ProcessCompStep', N'P') IS NOT NULL
    DROP PROCEDURE Core.ProcessCompStep
GO

CREATE PROCEDURE Core.ProcessCompStep @GameID varchar(40)
AS
    SET NOCOUNT ON
    DECLARE @NextCompStep int
    DECLARE @NextCompStepGenerator varchar(100)
    SELECT @NextCompStepGenerator = NextCompStepGenerator FROM Core.GameSession WHERE GameID = @GameID
    EXECUTE @NextCompStepGenerator @GameID, @NextCompStep OUTPUT
    EXECUTE Core.MakeStep @GameID, @NextCompStep, 'C'
GO

IF OBJECT_ID (N'Core.CalculateGameResult', N'P') IS NOT NULL
    DROP PROCEDURE Core.CalculateGameResult
GO

CREATE PROCEDURE Core.CalculateGameResult @GameID varchar(40)
AS
    IF EXISTS(SELECT GameResult FROM CORE.GameSession WHERE GameID = @GameID AND GameResult IS NOT NULL)
        RETURN
    DECLARE @Row1 char(3), @Row2 char(3), @Row3 char(3)
    DECLARE @Column1 char(3), @Column2 char(3), @Column3 char(3)
    DECLARE @DirectDiagonal char(3), @InverseDiagonal char(3)
    SET @Row1 = Core.GetRowValue(@GameID, 1)
    SET @Row2 = Core.GetRowValue(@GameID, 2)
    SET @Row3 = Core.GetRowValue(@GameID, 3)
    SET @Column1 = Core.GetColumnValue(@GameID, 1)
    SET @Column2 = Core.GetColumnValue(@GameID, 2)
    SET @Column3 = Core.GetColumnValue(@GameID, 3)
    SET @DirectDiagonal = Core.GetDirectDiagonalValue(@GameID)
    SET @InverseDiagonal = Core.GetInverseDiagonalValue(@GameID)
    IF (@Row1 = N'XXX' OR @Row2 = N'XXX' OR @Row3 = N'XXX' OR @Column1 = N'XXX' OR @Column2 = N'XXX' OR @Column3 = N'XXX' OR @DirectDiagonal = N'XXX' OR @InverseDiagonal = N'XXX')
    BEGIN
        UPDATE Core.GameSession SET GameResult = FirstPlayer WHERE GameID = @GameID
        RETURN
    END
    IF (@Row1 = N'OOO' OR @Row2 = N'OOO' OR @Row3 = N'OOO' OR @Column1 = N'OOO' OR @Column2 = N'OOO' OR @Column3 = N'OOO' OR @DirectDiagonal = N'OOO' OR @InverseDiagonal = N'OOO')
    BEGIN
        UPDATE Core.GameSession SET GameResult = IIF(FirstPlayer = 'U', 'C', 'U') WHERE GameID = @GameID
        RETURN
    END
    DECLARE @LogCount int
    SELECT @LogCount = COUNT(Number) FROM Core.GameSessionLog WHERE GameID = @GameID
    IF (@LogCount = 9)
        UPDATE Core.GameSession SET GameResult = 'D' WHERE GameID = @GameID
GO

IF OBJECT_ID(N'Core.ShowGameLog', N'P') IS NOT NULL
    DROP PROCEDURE Core.ShowGameLog
GO

CREATE PROCEDURE Core.ShowGameLog @GameID varchar(40)
AS
    SET NOCOUNT ON
    DECLARE @Number int
    DECLARE @Step int
    DECLARE @Value char(1)
    DECLARE LogCursor CURSOR READ_ONLY FOR SELECT Number, Step, Value FROM Core.GameSessionLog WHERE GameID = @GameID ORDER BY Number ASC
    OPEN LogCursor
    FETCH NEXT FROM LogCursor INTO @Number, @Step, @Value
    PRINT N'GAME LOG:'
    WHILE @@FETCH_STATUS = 0
    BEGIN
        PRINT N'Number = ' + STR(@Number, 1) + N', Row = ' + STR(@Step / 10, 1) + N', Column = ' + STR(@Step % 10, 1) + N', Value = ' + @Value
        FETCH NEXT FROM LogCursor INTO @Number, @Step, @Value
    END
    CLOSE LogCursor
    DEALLOCATE LogCursor
GO

IF OBJECT_ID(N'Core.ShowBoard', N'P') IS NOT NULL
    DROP PROCEDURE Core.ShowBoard
GO

CREATE PROCEDURE Core.ShowBoard @GameID varchar(40)
AS
    SET NOCOUNT ON
    PRINT REPLICATE(N'=', 13)
    PRINT N'|   |   |   |'
    PRINT N'| ' + Core.GetCellValue(@GameID, 11) + N' | ' + Core.GetCellValue(@GameID, 12) + N' | ' + Core.GetCellValue(@GameID, 13) + N' |'
    PRINT N'|   |   |   |'
    PRINT REPLICATE(N'=', 13)
    PRINT N'|   |   |   |'
    PRINT N'| ' + Core.GetCellValue(@GameID, 21) + N' | ' + Core.GetCellValue(@GameID, 22) + N' | ' + Core.GetCellValue(@GameID, 23) + N' |'
    PRINT N'|   |   |   |'
    PRINT REPLICATE(N'=', 13)
    PRINT N'|   |   |   |'
    PRINT N'| ' + Core.GetCellValue(@GameID, 31) + N' | ' + Core.GetCellValue(@GameID, 32) + N' | ' + Core.GetCellValue(@GameID, 33) + N' |'
    PRINT N'|   |   |   |'
    PRINT REPLICATE(N'=', 13)
GO

IF OBJECT_ID (N'CompStrategy.FindFreeCell', N'FN') IS NOT NULL
    DROP FUNCTION CompStrategy.FindFreeCell
GO

CREATE FUNCTION CompStrategy.FindFreeCell (@GameID varchar(40), @CellNumber int)
RETURNS int
AS
BEGIN
    DECLARE @Step int; /* ";" here only for CTE */
    WITH Board (Step)
    AS
    (
        SELECT 11 UNION ALL SELECT 12 UNION ALL SELECT 13
        UNION ALL
        SELECT 21 UNION ALL SELECT 22 UNION ALL SELECT 23
        UNION ALL
        SELECT 31 UNION ALL SELECT 32 UNION ALL SELECT 33
    ),
    Data (CellNumber, Step)
    AS
    (
        SELECT ROW_NUMBER() OVER(ORDER BY Board.Step ASC), Board.Step
        FROM Board LEFT OUTER JOIN Core.GameSessionLog AS GameLog ON Board.Step = GameLog.Step AND GameLog.GameID = @GameID
        WHERE GameLog.Value IS NULL
    )
    SELECT @Step = Step FROM Data WHERE CellNumber = @CellNumber
    RETURN @Step
END
GO


IF OBJECT_ID(N'CompStrategy.SimpleGenerateCompNextStep', N'P') IS NOT NULL
    DROP PROCEDURE CompStrategy.SimpleGenerateCompNextStep
GO

CREATE PROCEDURE CompStrategy.SimpleGenerateCompNextStep @GameID varchar(40), @NextStep int OUTPUT
AS
    SET NOCOUNT ON
    SET @NextStep = CompStrategy.FindFreeCell(@GameID, 1)
GO

IF OBJECT_ID(N'CompStrategy.RandomGenerateCompNextStep', N'P') IS NOT NULL
    DROP PROCEDURE CompStrategy.RandomGenerateCompNextStep
GO

CREATE PROCEDURE CompStrategy.RandomGenerateCompNextStep @GameID varchar(40), @NextStep int OUTPUT
AS
    DECLARE @FreeCellsCount int
    SELECT @FreeCellsCount = 9 - COUNT(Number) FROM Core.GameSessionLog WHERE GameID = @GameID
    DECLARE @SelectedCell int
    SET @SelectedCell =  1 + FLOOR(@FreeCellsCount * RAND())
    SET @NextStep = CompStrategy.FindFreeCell(@GameID, @SelectedCell)
GO

IF OBJECT_ID (N'CompStrategy.FindMandatoryStep', N'FN') IS NOT NULL
    DROP FUNCTION CompStrategy.FindMandatoryStep
GO

CREATE FUNCTION CompStrategy.FindMandatoryStep (@GameID varchar(40), @Value char(1))
RETURNS int
AS
BEGIN
    DECLARE @Pattern1 char(3), @Pattern2 char(3), @Pattern3 char(3)
    SET @Pattern1 = ' ' + @Value + @Value
    SET @Pattern2 = @Value + ' ' + @Value
    SET @Pattern3 = @Value + @Value + ' '
    DECLARE @Result int
    SET @Result = CASE Core.GetRowValue(@GameID, 1) WHEN @Pattern1 THEN 11 WHEN @Pattern2 THEN 12 WHEN @Pattern3 THEN 13 ELSE NULL END
    IF @Result IS NOT NULL
        RETURN @RESULT
    SET @Result = CASE Core.GetRowValue(@GameID, 2) WHEN @Pattern1 THEN 21 WHEN @Pattern2 THEN 22 WHEN @Pattern3 THEN 23 ELSE NULL END
    IF @Result IS NOT NULL
        RETURN @RESULT
    SET @Result = CASE Core.GetRowValue(@GameID, 3) WHEN @Pattern1 THEN 31 WHEN @Pattern2 THEN 32 WHEN @Pattern3 THEN 33 ELSE NULL END
    IF @Result IS NOT NULL
        RETURN @RESULT
    SET @Result = CASE Core.GetColumnValue(@GameID, 1) WHEN @Pattern1 THEN 11 WHEN @Pattern2 THEN 21 WHEN @Pattern3 THEN 31 ELSE NULL END
    IF @Result IS NOT NULL
        RETURN @RESULT
    SET @Result = CASE Core.GetColumnValue(@GameID, 2) WHEN @Pattern1 THEN 12 WHEN @Pattern2 THEN 22 WHEN @Pattern3 THEN 32 ELSE NULL END
    IF @Result IS NOT NULL
        RETURN @RESULT
    SET @Result = CASE Core.GetColumnValue(@GameID, 3) WHEN @Pattern1 THEN 13 WHEN @Pattern2 THEN 23 WHEN @Pattern3 THEN 33 ELSE NULL END
    IF @Result IS NOT NULL
        RETURN @RESULT
    SET @Result = CASE Core.GetDirectDiagonalValue(@GameID) WHEN @Pattern1 THEN 11 WHEN @Pattern2 THEN 22 WHEN @Pattern3 THEN 33 ELSE NULL END
    IF @Result IS NOT NULL
        RETURN @RESULT
    SET @Result = CASE Core.GetInverseDiagonalValue(@GameID) WHEN @Pattern1 THEN 13 WHEN @Pattern2 THEN 22 WHEN @Pattern3 THEN 31 ELSE NULL END
    IF @Result IS NOT NULL
        RETURN @RESULT
    RETURN NULL
END
GO

IF OBJECT_ID (N'CompStrategy.FindOppositeCornerStep', N'FN') IS NOT NULL
    DROP FUNCTION CompStrategy.FindOppositeCornerStep
GO

CREATE FUNCTION CompStrategy.FindOppositeCornerStep (@GameID varchar(40), @Step int)
RETURNS int
AS
BEGIN
    IF @Step = 11
        RETURN IIF(Core.GetCellValue(@GameID, 33) = ' ', 33, NULL)
    IF @Step = 13
        RETURN IIF(Core.GetCellValue(@GameID, 31) = ' ', 31, NULL)
    IF @Step = 31
        RETURN IIF(Core.GetCellValue(@GameID, 13) = ' ', 13, NULL)
    IF @Step = 33
        RETURN IIF(Core.GetCellValue(@GameID, 11) = ' ', 11, NULL)
    IF @Step = 12
        RETURN IIF(Core.GetCellValue(@GameID, 31) = ' ', 31, IIF(Core.GetCellValue(@GameID, 33) = ' ', 33, NULL))
    IF @Step = 32
        RETURN IIF(Core.GetCellValue(@GameID, 11) = ' ', 11, IIF(Core.GetCellValue(@GameID, 13) = ' ', 13, NULL))
    IF @Step = 21
        RETURN IIF(Core.GetCellValue(@GameID, 13) = ' ', 13, IIF(Core.GetCellValue(@GameID, 33) = ' ', 33, NULL))
    IF @Step = 23
        RETURN IIF(Core.GetCellValue(@GameID, 11) = ' ', 11, IIF(Core.GetCellValue(@GameID, 31) = ' ', 31, NULL))
    RETURN NULL
END
GO

IF OBJECT_ID (N'CompStrategy.GetNthStep', N'FN') IS NOT NULL
    DROP FUNCTION CompStrategy.GetNthStep
GO

CREATE FUNCTION CompStrategy.GetNthStep (@GameID varchar(40), @StepNumber int)
RETURNS int
AS
BEGIN
    DECLARE @Step int
    SELECT @Step = Step FROM Core.GameSessionLog WHERE (GameID = @GameID) AND (Number = @StepNumber)
    RETURN @Step
END
GO

IF OBJECT_ID(N'CompStrategy.SmartGenerateCompNextStep', N'P') IS NOT NULL
    DROP PROCEDURE CompStrategy.SmartGenerateCompNextStep
GO

CREATE PROCEDURE CompStrategy.SmartGenerateCompNextStep @GameID varchar(40), @NextStep int OUTPUT
AS
    SET NOCOUNT ON
    DECLARE @Value char(1)
    SET @Value = Core.GetCellValue(@GameID, 22)
    IF (@Value IS NULL)
    BEGIN
        SET @NextStep = 22
        RETURN
    END
    DECLARE @Figure char(1), @OppositeFigure char(1)
    SELECT @Figure = IIF(FirstPlayer = 'C', 'X', 'O'), @OppositeFigure = IIF(FirstPlayer = 'U', 'X', 'O') FROM Core.GameSession WHERE GameID = @GameID
    SET @NextStep = CompStrategy.FindMandatoryStep(@GameID, @Figure)
    IF @NextStep IS NOT NULL
        RETURN
    SET @NextStep = CompStrategy.FindMandatoryStep(@GameID, @OppositeFigure)
    IF @NextStep IS NOT NULL
        RETURN
    IF @Figure = 'X'
    BEGIN
        DECLARE @LastStep int
        SELECT TOP 1 @LastStep = Step FROM Core.GameSessionLog WHERE GameID = @GameID ORDER BY Number DESC
        SET @NextStep = CompStrategy.FindOppositeCornerStep(@GameID, @LastStep)
        IF @NextStep IS NULL
            SET @NextStep = CompStrategy.FindFreeCell(@GameID, 1)
    END
    ELSE
    BEGIN
        DECLARE @StepCount int
        SELECT @StepCount = COUNT(Number) FROM Core.GameSessionLog WHERE GameID = @GameID
        DECLARE @FirstUserStep int, @SecondUserStep int
        SET @FirstUserStep = CompStrategy.GetNthStep(@GameID, 1)
        SET @SecondUserStep = CompStrategy.GetNthStep(@GameID, 3)
        IF @FirstUserStep = 22
            SET @NextStep = IIF(Core.GetCellValue(@GameID, 11) = ' ', 11, IIF(Core.GetCellValue(@GameID, 13) = ' ', 13, IIF(Core.GetCellValue(@GameID, 31) = ' ', 31, IIF(Core.GetCellValue(@GameID, 33) = ' ', 33, NULL))))
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 11)
            SET @NextStep = IIF(Core.GetCellValue(@GameID, 31) = ' ', 31, 13)
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 31)
            SET @NextStep = IIF(Core.GetCellValue(@GameID, 11) = ' ', 11, 33)
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 13)
            SET @NextStep = IIF(Core.GetCellValue(@GameID, 33) = ' ', 33, 11)
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 33)
            SET @NextStep = IIF(Core.GetCellValue(@GameID, 13) = ' ', 31, 13)
        ELSE IF (@StepCount = 3) AND (@FirstUserStep IN (12, 32, 21, 23)) AND (@SecondUserStep IN (11, 13, 31, 33))
            SET @NextStep = CompStrategy.FindOppositeCornerStep(@GameID, @SecondUserStep)
        ELSE IF (@StepCount = 3) AND (@FirstUserStep IN (12, 32, 21, 23)) AND (@SecondUserStep IN (12, 32, 21, 23))
            SET @NextStep = 11
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 12) AND (@SecondUserStep = 21)
            SET @NextStep = 11
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 12) AND (@SecondUserStep = 23)
            SET @NextStep = 13
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 21) AND (@SecondUserStep = 12)
            SET @NextStep = 11
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 21) AND (@SecondUserStep = 32)
            SET @NextStep = 31
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 32) AND (@SecondUserStep = 21)
            SET @NextStep = 31
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 32) AND (@SecondUserStep = 23)
            SET @NextStep = 33
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 23) AND (@SecondUserStep = 32)
            SET @NextStep = 33
        ELSE IF (@StepCount = 3) AND (@FirstUserStep = 23) AND (@SecondUserStep = 12)
            SET @NextStep = 13
        IF @NextStep IS NULL
            SET @NextStep = CompStrategy.FindFreeCell(@GameID, 1)
    END
GO


/* API */
IF OBJECT_ID(N'dbo.StartGame', N'P') IS NOT NULL
    DROP PROCEDURE dbo.StartGame
GO

CREATE PROCEDURE dbo.StartGame @GameID varchar(40), @IsUserFirst bit, @NextCompStepGenerator varchar(50)
AS
    SET NOCOUNT ON
    BEGIN TRANSACTION StartGame
    INSERT Core.GameSession(GameID, FirstPlayer, NextCompStepGenerator) VALUES(@GameID, IIF(@IsUserFirst = 1, 'U', 'C'), @NextCompStepGenerator)
    IF (@IsUserFirst <> 1)
        INSERT Core.GameSessionLog(GameID, Number, Step, Value) VALUES(@GameID, 1, 22, 'X')
    COMMIT TRANSACTION StartGame
GO

IF OBJECT_ID(N'dbo.StartSimpleCompGame', N'P') IS NOT NULL
    DROP PROCEDURE dbo.StartSimpleCompGame
GO

CREATE PROCEDURE dbo.StartSimpleCompGame @GameID varchar(40), @IsUserFirst bit
AS
    SET NOCOUNT ON
    EXECUTE dbo.StartGame @GameID, @IsUserFirst, 'CompStrategy.SimpleGenerateCompNextStep'
GO

IF OBJECT_ID(N'dbo.StartRandomCompGame', N'P') IS NOT NULL
    DROP PROCEDURE dbo.StartRandomCompGame
GO

CREATE PROCEDURE dbo.StartRandomCompGame @GameID varchar(40), @IsUserFirst bit
AS
    SET NOCOUNT ON
    EXECUTE dbo.StartGame @GameID, @IsUserFirst, 'CompStrategy.RandomGenerateCompNextStep'
GO

IF OBJECT_ID(N'dbo.StartSmartCompGame', N'P') IS NOT NULL
    DROP PROCEDURE dbo.StartSmartCompGame
GO

CREATE PROCEDURE dbo.StartSmartCompGame @GameID varchar(40), @IsUserFirst bit
AS
    SET NOCOUNT ON
    EXECUTE dbo.StartGame @GameID, @IsUserFirst, 'CompStrategy.SmartGenerateCompNextStep'
GO

IF OBJECT_ID(N'dbo.FinishGame', N'P') IS NOT NULL
    DROP PROCEDURE dbo.FinishGame
GO

CREATE PROCEDURE dbo.FinishGame @GameID varchar(40)
AS
    SET NOCOUNT ON
    BEGIN TRANSACTION FinishGame
    DELETE FROM Core.GameSession WHERE GameID = @GameID
    COMMIT TRANSACTION FinishGame
GO

IF OBJECT_ID(N'dbo.ShowInfo', N'P') IS NOT NULL
    DROP PROCEDURE dbo.ShowInfo
GO

CREATE PROCEDURE dbo.ShowInfo @GameID varchar(40)
AS
    SET NOCOUNT ON
    EXECUTE Core.ShowBoard @GameID
    EXECUTE Core.ShowGameLog @GameID
    DECLARE @GameResult varchar(20)
    SET @GameResult = dbo.GetGameResult(@GameID)
    IF (@GameResult IS NOT NULL)
        PRINT N'Game is finished. ' + @GameResult
GO


IF OBJECT_ID (N'dbo.GetGameResult', N'FN') IS NOT NULL
    DROP FUNCTION dbo.GetGameResult
GO

CREATE FUNCTION dbo.GetGameResult (@GameID varchar(40))
RETURNS varchar(20)
AS
BEGIN
    DECLARE @GameResult char(1)
    SELECT @GameResult = GameResult FROM Core.GameSession WHERE GameID = @GameID
    DECLARE @Result varchar(20)
    RETURN CASE @GameResult
               WHEN 'U' THEN 'User is winner'
               WHEN 'C' THEN 'Computer is winner'
               WHEN 'D' THEN 'Result is draw'
               ELSE NULL
           END
END
GO

IF OBJECT_ID('dbo.ProcessStep', 'P') IS NOT NULL
    DROP PROCEDURE dbo.ProcessStep
GO

CREATE PROCEDURE dbo.ProcessStep @GameID varchar(40), @UserStepRow int, @UserStepColumn int
AS
    SET NOCOUNT ON
    BEGIN TRANSACTION ProcessStep
    IF (dbo.GetGameResult(@GameID) IS NOT NULL)
    BEGIN
        EXECUTE dbo.ShowInfo @GameID
        COMMIT TRANSACTION ProcessStep
        RETURN
    END
    BEGIN TRY
        DECLARE @UserStep int
        SET @UserStep = 10 * @UserStepRow + @UserStepColumn
        EXECUTE Core.ProcessUserStep @GameID, @UserStep
    END TRY
    BEGIN CATCH
        DECLARE @ErrorMessage nvarchar(4000)
        DECLARE @ErrorSeverity int
        DECLARE @ErrorState int
        SELECT @ErrorMessage = ERROR_MESSAGE(), @ErrorSeverity = ERROR_SEVERITY(), @ErrorState = ERROR_STATE()
        RAISERROR (@ErrorMessage, @ErrorSeverity, @ErrorState)
        ROLLBACK TRANSACTION ProcessStep
        RETURN
    END CATCH
    EXECUTE Core.CalculateGameResult @GameID
    IF (dbo.GetGameResult(@GameID) IS NOT NULL)
    BEGIN
        EXECUTE dbo.ShowInfo @GameID
        COMMIT TRANSACTION ProcessStep
        RETURN
    END
    EXECUTE Core.ProcessCompStep @GameID
    EXECUTE Core.CalculateGameResult @GameID
    EXECUTE dbo.ShowInfo @GameID
    COMMIT TRANSACTION ProcessStep
GO

IF OBJECT_ID(N'dbo.Cleanup', N'P') IS NOT NULL
    DROP PROCEDURE dbo.Cleanup
GO

CREATE PROCEDURE dbo.Cleanup
AS
    SET NOCOUNT ON
    BEGIN TRANSACTION Cleanup
    DELETE FROM Core.GameSession
    COMMIT TRANSACTION Cleanup
GO

/*
EXECUTE StartSimpleCompGame N'AA', 1;
EXECUTE ShowInfo N'AA';
EXECUTE ProcessStep N'AA', 2, 2;
EXECUTE ProcessStep N'AA', 1, 1;
EXECUTE ProcessStep N'AA', 1, 3;
EXECUTE ProcessStep N'AA', 3, 1;
EXECUTE FinishGame N'AA';
*/

/*
EXECUTE StartSimpleCompGame N'BB', 0;
EXECUTE ShowInfo N'BB';
EXECUTE ProcessStep N'BB', 1, 2;
EXECUTE ProcessStep N'BB', 1, 3;
EXECUTE ProcessStep N'BB', 2, 3;
EXECUTE FinishGame N'BB';
*/

/*
EXECUTE StartSimpleCompGame N'CC', 0;
EXECUTE ShowInfo N'CC';
EXECUTE ProcessStep N'CC', 1, 2;
EXECUTE ProcessStep N'CC', 3, 3;
EXECUTE ProcessStep N'CC', 3, 1;
EXECUTE ProcessStep N'CC', 2, 3;
EXECUTE FinishGame N'CC';
*/

/*
EXECUTE StartRandomCompGame N'AA', 1;
EXECUTE ShowInfo N'AA';
EXECUTE ProcessStep N'AA', 2, 2;
EXECUTE ProcessStep N'AA', 1, 1;
EXECUTE ProcessStep N'AA', 3, 3;
EXECUTE FinishGame N'AA';
*/

/*
EXECUTE dbo.Cleanup
*/

/*
EXECUTE StartSmartCompGame N'AA', 1;
EXECUTE ShowInfo N'AA';
EXECUTE ProcessStep N'AA', 2, 2;
EXECUTE ProcessStep N'AA', 1, 3;
EXECUTE ProcessStep N'AA', 2, 1;
EXECUTE ProcessStep N'AA', 3, 2;
EXECUTE ProcessStep N'AA', 3, 3;
EXECUTE FinishGame N'AA';
*/

/*
EXECUTE StartSmartCompGame N'AA', 0;
EXECUTE ShowInfo N'AA';
EXECUTE ProcessStep N'AA', 1, 2;
EXECUTE ProcessStep N'AA', 1, 3;
EXECUTE ProcessStep N'AA', 2, 1;
EXECUTE FinishGame N'AA';
*/