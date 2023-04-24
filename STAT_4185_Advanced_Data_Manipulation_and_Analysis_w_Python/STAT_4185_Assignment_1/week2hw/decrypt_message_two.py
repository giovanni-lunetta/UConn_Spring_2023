encrypted_file = open("week2hw/encrypted_message_two.txt", 'r')

encrypted_message = encrypted_file.readline()

encrypted_file.close()

# Write Code Here
# Create an empty string to store the decrypted message
decrypted_message = ""

# Iterate over the characters in the encrypted message
for i in range(0, len(encrypted_message)//2):
    # Add every second character to the decrypted message
    decrypted_message += encrypted_message[i*2]
    # Add the corresponding character from the opposite end of the message
    decrypted_message += encrypted_message[-i*2-2]

print(decrypted_message)