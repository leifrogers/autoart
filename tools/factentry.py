'''add a fact -> new facts added to factoids then added to autoart.clp'''
import datetime
import string
import os

filename = "factoids.clp"
preload = open(filename, "r")
lines = [line.rstrip('\n') for line in preload]


# there's better ways of formatting strings than below but i'm not
# in the mood to deal with it for the moment
moods = ["1 depressed     ",
         "2 blue          ",
         "3 melancholy    ",
         "4 bored         ",
         "5 reflective    ",
         "6 nostalgic     ",
         "7 happy         ",
         "8 curious       ",
         "9 dreamy        ",
         "10 mad          ",
         "11 sad          ",
         "12 angry        ",
         "13 indifferent  ",
         "14 disassociated"]

choice_slots = ["1: Person           ",
                "2: General Location ",
                "3: Detailed Location",
                "4: Mood             ",
                "5: Mood Strength    ",
                "6: Time of Day      ",
                "7: influence_person ",
                "8: influence_other  ",
                "9: Memory           ",
                "q: Quit             "]


def fmtcols(mylist, cols):
    lines = ("\t".join(mylist[i:i+cols]) for i in range(0, len(mylist), cols))
    return '\n'.join(lines)


def writefact(fact):
    test = [fact.rstrip('\n')]
    dupes = set(test) & set(lines)
    if len(dupes) < 1:
        file = open(filename, "a")
        file.write(fact)
        file.close()
        input("Fact entered - hit any key to continue")
        main()
    else:
        input("Fact already exists - hit any key to continue")
        main()


# (deftemplate person
#  (slot firstname)
#  (slot lastname)
#  (slot connection)
#  (slot relationship)
#  (multislot association))

def person():
    firstname = input("First Name: ")
    lastname = input("Last Name: ")
    connection = input("Connection: ")
    relationship = input("Relationship: ")
    print(fmtcols(moods, 2))
    association = input("Enter mood association numbers: ")
    fact = "(person (firstname {})(lastname {})(connection {})(relationship {})(association {}))\n".format(
        firstname, lastname, connection, relationship, association)
    writefact(fact)


# (deftemplate general-locations
#  (slot location)
#  (multislot association))

def general_location():
    location = input(
        "Enter a general location (city, building, state, area, etc.): ")
    print(fmtcols(moods, 2))
    association = input("Enter mood association numbers: ")
    fact = "(general-locations (location {}) (association {}))\n".format(location, association)
    writefact(fact)


# (deftemplate detailed-locations
#  (slot general-location)
#  (slot detailed-location)
#  (slot foreground)
#  (slot background)
#  (multislot details)
#  (multislot association))

def detailed_location():
    location = input(
        "Enter a general location (city, building, state, area, etc.): ")
    detailed_location = input(
        "Enter the detailed location (house, restaurant, store, park, etc.):")
    foreground = input("Enter a foreground detail: ")
    background = input("Enter a background detail: ")
    details = input("Enter other details (multislot field): ")
    print(fmtcols(moods, 2))
    association = input("Enter mood association numbers: ")
    fact = "(detailed-locations (general-location {})(detailed-location {})(foreground \"{}\")(background \"{}\")(details {})(association {}))\n".format(
        location, detailed_location, foreground, background, details, association)
    writefact(fact)

# (deftemplate mood
#  (slot typeof))


def mood():
    mood = input(
        "Enter a mood: ")
    fact = "(mood (typeof {}))\n".format(mood)
    writefact(fact)


# (deftemplate mood-strength
# (slot strength))

def mood_strength():
    moodstrength = input(
        "Enter a mood: ")
    fact = "(mood-strength (strength {}}))\n".format(moodstrength)
    writefact(fact)


# (deftemplate senses
# (slot typeof)
# (slot influence))

# (deftemplate time-of-day
# (slot time)
# (multislot association))

def time_of_day():
    timeofday = input(
        "Enter a time (noon, breakfast, midnight, 2pm, etc): ")
    print(fmtcols(moods, 2))
    association = input("Enter mood association numbers: ")
    fact = "(time-of-day (time {}) (association {}))\n".format(timeofday, association)
    writefact(fact)

# (deftemplate influence-person
# (slot firstname)
# (slot lastname)
# (slot typeof))


def influence_person():
    firstname = input("First Name: ")
    lastname = input("Last Name: ")
    typeof = input("Type of influence (artist, actor, war_hero, etc.): ")
    fact = "(influence-person (firstname {})(lastname {})(typeof {}))\n".format(
        firstname, lastname, typeof)
    writefact(fact)

# (deftemplate influence-other
# (slot typeof)
# (multislot comments))


def influence_other():
    typeof = input("Type of influence (building, place, movie, book, etc.): ")
    comments = input("Comments (multislot field): ")
    fact = "(influence-person (typeof {})(comments {}))\n".format(
        typeof, comments)
    writefact(fact)
# (deftemplate global-subject
# (slot subject))

# (deftemplate detailed-subject
# (slot subject)
# (slot location)
# (slot foreground)
# (slot background)
# (slot palette)
# (multislot accessories))

# (deftemplate global-medium
# (slot medium))

# (deftemplate detailed-medium
# (slot typeof)
# (slot medium))

# (deftemplate memory
#  (slot description)
#  (multislot association))


def memory():
    description = input("Short description of memory: ")
    association = input("Enter mood association numbers: ")
    fact = "(memory (description \"{}\")(association {}))\n".format(
        description, association)
    writefact(fact)


def default():
    print("please choose a valid choice")
    main()


def choose(choice):
    return choices.get(choice, default)()


choices = {
    "1": person,
    "2": general_location,
    "3": detailed_location,
    "4": mood,
    "5": mood_strength,
    "6": time_of_day,
    "7": influence_person,
    "8": influence_other,
    "9": memory,
    "q": quit
}


def main():
    print(";;;================================")
    print(";;;Select a fact template to enter:")
    print(";;;================================")
    print(fmtcols(choice_slots, 2))
    template_choice = input()
    choose(template_choice)


main()
