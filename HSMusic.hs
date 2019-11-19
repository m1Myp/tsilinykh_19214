data Group = ToGroup {
	groupName :: String,
	groupIsFavorite :: Bool
} deriving (Show, Eq)

data Song = ToSong {
	songName :: String,
	songGroup :: Group,
	songIsFavorite :: Bool
} deriving (Show, Eq)

data Playlist = ToPlaylist {
	playlistName :: String,
	playlistSongs :: [Song],
	playlistIsFavorite :: Bool
} deriving (Show, Eq)

data User = ToUser {
	userName :: String,
	myPlaylists :: [Playlist],
	mySongs :: [Song]
} deriving (Show, Eq)

createUser :: String -> User
createUser name = ToUser name [] []

createGroup :: String -> Group
createGroup name = ToGroup name False

createSong :: String -> Group -> Song
createSong nameSong nameGroup = ToSong nameSong nameGroup False

createPlaylist :: String -> Playlist
createPlaylist namePlaylist = ToPlaylist namePlaylist [] False
	
addToPlaylist :: Playlist -> Song -> Playlist
addToPlaylist namePlaylist nameSong = ToPlaylist (playlistName namePlaylist) (nameSong : playlistSongs namePlaylist) False

addUserSong :: User -> Song -> User
addUserSong name nameSong = ToUser (userName name) (myPlaylists name) (nameSong : mySongs name)

addUserPlaylist :: User -> Playlist -> User
addUserPlaylist name namePlaylist = ToUser (userName name) (namePlaylist : myPlaylists name) (mySongs name)
{-
findMusicByGroup :: Song -> [Song]


findMusicByPlaylist ::

addToFavorite :: 

showFavorite ::
-}


