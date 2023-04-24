cipher = {
    'a':'v',
    'b':'h',
    'c':'r',
    'd':'j',
    'e':'t',
    'f':'x',
    'g':'s',
    'h':'a',
    'i':'e',
    'j':'f',
    'k':'b',
    'l':'n',
    'm':'o',
    'n':'i',
    'o':'g',
    'p':'l',
    'q':'m',
    'r':'z',
    's':'q',
    't':'u',
    'u':'c',
    'v':'k',
    'w':'d',
    'x':'p',
    'y':'y',
    'z':'w',
    ' ': '}',
    '\n': '^',
    ',': '5',
    '!': '[',
    ':':'-',
    ')':'*',
    '.': '%' 
}

encrypted_file = open("week2hw/encrypted_message_one.txt", 'r')

encrypted_message = encrypted_file.readline()

encrypted_file.close()

# Write code below
def decrypt(encrypted_message, cipher):
    decrypted_message = ""
    for char in encrypted_message:
        for key, value in cipher.items():
            if char == value:
                decrypted_message += key
    return decrypted_message

print(decrypt(encrypted_message, cipher))