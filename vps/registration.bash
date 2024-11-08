NONCE="$(curl http://localhost:8008/_synapse/admin/v1/register | jq --raw-output '.nonce')"
USERNAME="mrnobody"
PASSWORD='A5j6*5#9!N%982$$4$l4Q9@*!5ynKm5j'
SECRET='Zn2*3!t24S8^%3%i9t$d2K34bF5lS!^$'
DISPLAYNAME="MrNobody"

MAC=$(printf '%s\0%s\0%s\0%s' "$NONCE" "$USERNAME" "$PASSWORD" "admin" | openssl sha1 -hmac "$SECRET" | awk '{print $2}')

curl -X POST -H "Content-Type: application/json" -d "{ \"nonce\": \"$NONCE\", \"username\": \"$USERNAME\", \"displayname\": \"$DISPLAYNAME\", \"password\": \"$PASSWORD\", \"admin\": true, \"mac\": \"$MAC\" }" http://localhost:8008/_synapse/admin/v1/register
