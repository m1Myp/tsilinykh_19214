data Song = ToSong {
	songName :: String
} deriving (Show, Eq) --В изначале без авторства

data Group = ToGroup {
	groupName :: String,
	groupSongs :: [Song]
} deriving (Show, Eq) --Добавим авторство

data Playlist = ToPlaylist {
	playlistName :: String,
	playlistSongs :: [Song]
} deriving (Show, Eq) --Объеденим в плейлисты

data User = ToUser {
	userName :: String,
	myGroup :: [Group],
	myPlaylists :: [Playlist],
	mySongs :: [Song]
} deriving (Show, Eq) --Ета ты

createUser :: String -> User
createUser name = ToUser name [] [] []

createSong :: String -> Song
createSong nameSong = ToSong nameSong

createGroup :: String -> Group
createGroup name = ToGroup name []

addToGroup :: Group -> Song -> Group
addToGroup nameGroup nameSong = ToGroup (groupName nameGroup) (nameSong : groupSongs nameGroup)

createPlaylist :: String -> Playlist
createPlaylist namePlaylist = ToPlaylist namePlaylist []
	
addToPlaylist :: Playlist -> Song -> Playlist
addToPlaylist namePlaylist nameSong = ToPlaylist (playlistName namePlaylist) (nameSong : playlistSongs namePlaylist)

addUserSong :: User -> Song -> User
addUserSong name nameSong = ToUser (userName name) (myGroup name) (myPlaylists name) (nameSong : mySongs name)

addUserPlaylist :: User -> Playlist -> User
addUserPlaylist name namePlaylist = ToUser (userName name) (myGroup name) (namePlaylist : myPlaylists name) (mySongs name)

addUserGroup :: User -> Group -> User
addUserGroup name nameGroup = ToUser (userName name) (nameGroup : myGroup name) (myPlaylists name) (mySongs name)

findSongByGroup :: Group -> [Song]
findSongByGroup nameGroup = groupSongs nameGroup

findSongByPlaylist :: Playlist -> [Song]
findSongByPlaylist namePlaylist = playlistSongs namePlaylist

