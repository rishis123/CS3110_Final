# Design Document

## Persistence
There are two types of data that can be stored:
### Encryptables (passwords, logins)

Encryptables are stored in the file `data/encrypted/encryptables.json`. This file contains JSON with the following schema (also found in [schemas/encryptables-schema.json](schemas/encryptables-schema.json)):
```json
{
    "type": "array",
    "items": {
        "type": "object",
        "properties": {
            "name": {
                "type": "string",
                "description": "The name of the entry"
            },
            "encrypted": {
                "type": "string",
                "description": "An encrypted string representing the entry"
            }
        },
        "required": [
            "name", "encrypted"
        ],
        "additionalProperties": false
    }
}
```

That is, it is a list of encrypted strings indexed by their name.

### Unencryptables (salted hash of the master password)
Since there is only one unencryptable, the salted hash of the master password, it is simply stored on the first line of `data/unencrypted/masterpwd`