module Main where
import System.IO
import System.Process
import System.Random
import Control.Concurrent

kleineterz = 3
grosseterz = 4
quint = 7


data DreiklangTyp = Dur | Moll | Vermindert | Uebermaessig deriving (Show, Enum, Eq)

instance Random DreiklangTyp where
    random rgen = let (n, g) = next rgen
                      t = toEnum (n `mod` 4)
                  in (t, g)
    randomR (from, to) rgen = let (n, g) = next rgen
                                  to_ = fromEnum to
                                  from_ = fromEnum from
                                  t = toEnum ((n `mod` (to_ - from_)) + from_)
                              in (t, g)

second_pitch = [grosseterz, kleineterz, kleineterz, grosseterz]
third_pitch  = [quint, quint, kleineterz+kleineterz, grosseterz+grosseterz]

random_dreiklang :: IO (Music, DreiklangTyp) 
random_dreiklang = do
  startPitch <- getStdRandom (randomR (38,70))
  typ <- getStdRandom random 
  (let snd_p = (second_pitch !! (fromEnum typ)) + startPitch
       thrd_p = (third_pitch !! (fromEnum typ)) + startPitch
       chord = Note (pitch startPitch) 2 :=: (Rest (1/16) :+: Note (pitch snd_p) 2 :=: (Rest (2/16) :+: Note (pitch thrd_p) 2))
   in return (chord, typ))


main :: IO ()
main = do
  (inp, out, err, pid) <- start_stk_demo
  main_loop inp
  putStrLn "Wunderbar! bis zum naechsten mal..."
  terminateProcess pid
  where main_loop inp = do
            putStrLn "Rate Dreiklang, druecke Enter wenn bereit..."
            _ <- getLine
            (m,was) <- random_dreiklang
            weiter <- spiele_und_rate inp m was
            (if weiter == True then main_loop inp
                               else return ())
        spiele_und_rate :: Handle -> Music -> DreiklangTyp -> IO Bool
        spiele_und_rate inp m was_ist_es = do 
                      mapM_ (hPutStrLn inp) $ music_to_skini m 60
                      hFlush inp
                      putStrLn "(d)ur/(m)oll/(u)ebermaessig/(v)ermindert/(w)iederholen/(e)nde/(a)ufgeben"
                      (c:_) <- getLine
                      case c of  
                        'a' -> do putStrLn $ "Das war " ++ (show was_ist_es) 
                                  return True

                        'e' -> return False

                        'w' -> do
                               putStrLn "Nochmal raten..."
                               spiele_und_rate inp m was_ist_es

                        _ -> let geraten = case c of 'd' -> Dur
                                                     'm' -> Moll
                                                     'u' -> Uebermaessig
                                                     'v' -> Vermindert
                                                     in
                                                       if geraten == was_ist_es then do 
                                                             putStrLn "Richtig!!!!!!"
                                                             return True
                                                       else do
                                                             putStrLn "Leider Falsch :("
                                                             spiele_und_rate inp m was_ist_es
                                    


start_stk_demo = runInteractiveCommand "/usr/bin/stk-demo Plucked -or -ip -n 4"  

terz    = 3
gr_terz = 4
quarte  = 5
quinte  = 7


rpt 0 m = Rest 0
rpt n m = m :+: rpt (n-1) m 

rptm f 0 m = Rest 0
rptm f n m = m :+: rptm f (n - 1) (f m)

rep f g 0 m = Rest 0
rep f g n m = m :=: g (rep f g (n-1) (f m))

--  functional music basics, based on the school of expression lecture slides 
data Music = Note Pitch Dur 
	| Rest Dur
	| Music :+: Music
	| Music :=: Music
	| Tempo Dur Music
	| Trans Int Music
	| Instr Int Music
	| Control ControlMessage Music
	deriving (Show, Eq)

type Dur = Rational

type Pitch = (Tone, Octave)
data Tone = C | Cis | D | Dis | E | F | Fis | G | Gis | A | B | H
	deriving (Show, Eq)
type Octave = Int

type ControlMessage = (Int, Int)

-- absolute pitches
type AbsPitch = Int

trans :: Int -> Pitch -> Pitch
trans i p = pitch (absPitch p + i)

pitch :: AbsPitch -> Pitch
pitch p = ([C, Cis, D, Dis, E, F, Fis, G, Gis, A, B, H] !! mod p 12, p `quot` 12)

absPitch :: Pitch -> AbsPitch
absPitch (n, o) = (o * 12) + (case n of C -> 0; Cis -> 1; D -> 2; Dis -> 3; E -> 4; F -> 5; Fis -> 6; G -> 7; Gis -> 8; A -> 9; B -> 10; H -> 11)

-- delay
delay :: Dur -> Music -> Music
delay d = ((Rest d) :+:)

-- absolute durations
abs_dur :: BeatPM -> Dur -> AbsDur
abs_dur bpm d = (fromRational d) / (bpm / (60.0)) 

type BeatPM = Float
type AbsDur = Float

-- calculate the duration of a piece of music
duration :: Music -> Dur
duration m = case m of
	Rest d -> d
	Note _ d -> d
	m1 :+: m2 -> duration m1 + duration m2
	m1 :=: m2 -> max (duration m1) (duration m2) 
	Tempo d _ -> d
	Trans _ m1 -> duration m1
	Instr _ m1 -> duration m1
	Control _ m1 -> duration m1
	
-- convert to a simple event stream of musical events with absolute values 
-- that soley consist of NoteOn and NoteOff events with midi notes and timestamps
data MusicEvent = MusicEv MusicEventType AbsPitch AbsDur
	deriving (Show, Eq)

data MusicEventType = NoteOn | NoteOff
	deriving (Show, Eq)

to_event_stream :: BeatPM -> Music -> AbsDur -> (AbsDur, [MusicEvent])
to_event_stream bpm um start_time = 
     case m of
	Note p d -> 
		let dur = start_time + (abs_dur bpm d) 
		in (dur, [(MusicEv NoteOn (absPitch p) start_time), (MusicEv NoteOff (absPitch p) dur)]) 
	
	Rest d ->
		(start_time  + (abs_dur bpm d), [])

	Tempo d m1 ->
		to_event_stream (bpm * (fromRational (duration m1)) * 1.0/(fromRational d)) m1 start_time

	m1 :+: m2 ->
		(total_dur,  evt_str1 ++ evt_str2)
		where (st1, evt_str1) = to_event_stream bpm m1 start_time
		      (total_dur, evt_str2) = to_event_stream bpm m2 st1

	m1 :=: m2 ->
		(max dur1 dur2, merge evt_str1 evt_str2)
		where (dur1, evt_str1) = to_event_stream bpm m1 start_time
		      (dur2, evt_str2) = to_event_stream bpm m2 start_time

	_ -> (start_time, [])

     where 
	m = transpose_all um 0
	transpose_all :: Music -> Int -> Music
	transpose_all m tr = case m of 
		Trans tr2 m1 -> transpose_all m1 (tr + tr2)
		Note p d -> Note (trans tr p) d
		m1 :+: m2 -> (transpose_all m1 tr) :+: (transpose_all m2 tr)
		m1 :=: m2 -> (transpose_all m1 tr) :=: (transpose_all m2 tr)
                Tempo d m1 -> Tempo d (transpose_all m1 tr)
		other -> other
	merge :: [MusicEvent] -> [MusicEvent] -> [MusicEvent]
	merge l1 l2 = mergea l1 l2 []
	mergea :: [MusicEvent] -> [MusicEvent] -> [MusicEvent] -> [MusicEvent]
	mergea [] l2 acc = acc ++ l2
	mergea l1 [] acc = acc ++ l1
	mergea l1@(e1@(MusicEv _ _ dur1):r1) l2@(e2@(MusicEv _ _ dur2):r2) acc 
		| dur1 <=  dur2 = mergea r1 l2 (acc ++ [e1])   
		| dur1 >   dur2 = mergea l1 r2 (acc ++ [e2])   

-- convert to skinni
{- 
<type> <time> <channel> <arg1> <arg2>
NoteOn delay channel midi-note volume
NoteOff delay channel midi-note volume
time is always the time to wait after the last event
arg1 is the midi note number
arg2 is the volume
-}
to_skini :: [MusicEvent] -> [[Char]]
to_skini event_stream = skini_stream
	where	(time_acc, skini_stream) = foldl make_skini (0.0, []) event_stream
		make_skini :: (AbsDur, [[Char]]) -> MusicEvent -> (AbsDur, [[Char]])
		make_skini (total_time, other_skini) (MusicEv mtype pitch ev_time)  = 
			(ev_time, other_skini ++ [(show mtype) ++ "   " ++ (show (ev_time - total_time)) ++ "   1   " ++ (show pitch) ++ "   127.0"]) 

music_to_skini :: Music -> BeatPM -> [[Char]]
music_to_skini m bpm = to_skini event_stream		
	where (_, event_stream) = to_event_stream bpm m 0.0
