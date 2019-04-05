-- This file stores a Config type (analogous to a "schema")
let Environment = < Development : {} | Production : {} | Test : {} >

let Config
	: Type
	= { _cAppName :
		  Text
	  , _cEnvironment :
		  Environment
	  , _cPort :
		  Natural
	  , _cSalt :
		  Text
	  , _cDatabase :
		  { _dbUser : Text, _dbName : Text, _dbHost : Text }
	  }

in  Config
