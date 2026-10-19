def word_count(text):
    words = text.split(" ")
    return len(words)

def report(text):
    count = word_count(text)
    return count

print(report("the quick brown fox"))
