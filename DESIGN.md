# Design Document

## Persistence
There are two types of data that can be stored:
### Encryptables (passwords, logins)

Encryptables are stored in the file `data/encrypted/encryptables.json`. Each line of the file contains a JSON object with the following schema (also found in [schemas/encryptables-schema.json](schemas/encryptables-schema.json)):
```json
{
    "type": "object",
    "properties": {
        "form": {
            "enum": [
                "login",
                "password"
            ],
            "description": "The type of the entry"
        },
        "name": {
            "type": "string",
            "description": "The name of the entry"
        },
        "encrypted_data": {
            "type": "string",
            "description": "An encrypted string representing the entry"
        }
    },
    "required": [
        "form",
        "name",
        "encrypted_data"
    ],
    "additionalProperties": false
}
```

That is, each object contains a type (login or password), its name, and an encrypted string containing the actual data.

### Unencryptables (salted hash of the master password)
Since there is only one unencryptable, the salted hash of the master password, it is simply stored on the first line of `data/unencrypted/masterpwd`.