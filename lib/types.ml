type unencryptable = MasterPasswordHash of string

type encryptable =
  | Password of {
      name : string;
      password : string;
    }
  | Login of {
      name : string;
      username : string;
      password : string;
      url : string option;
    }
