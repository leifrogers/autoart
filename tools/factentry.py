'''add a fact - super preliminary - pay no attention'''
import datetime
import os

filename = "factoids.clp"


def writefact(fact):
    print(fact)
    file = open(filename, "a")
    file.write(fact)
    file.close()
    main()


# (deftemplate person
#  (slot firstname)
#  (slot lastname)
#  (slot connection)
#  (slot relationship))

def person():
    firstname = input("First Name: ")
    lastname = input("Last Name: ")
    connection = input("Connection: ")
    relationship = input("Relationship: ")
    fact = "(person (firstname " + firstname + ")(lastname " + lastname + \
        ")(connection " + connection + \
        ")(relationship " + relationship + "))\n"
    writefact(fact)


# (deftemplate general-locations
#  (slot location))

def general_location():
    location = input(
        "Enter a general location (city, building, state, area, etc.): ")
    fact = "(general-locations (location{}}))\n".format(location)
    writefact(fact)


# (deftemplate detailed-locations
#  (slot general-location)
#  (slot detailed-location)
#  (slot foreground)
#  (slot background)
#  (multislot details))

def detailed_location():
    location = input(
        "Enter a general location (city, building, state, area, etc.): ")
    detailed_location = input(
        "Enter the detailed location (house, restaurant, store, park, etc.):")
    foreground = input("Enter a foreground detail: ")
    background = input("Enter a background detail: ")
    details = input("Enter other details (multislot field): ")
    fact = "(detailed-locations (general-location {})(detailed-location {})(foreground {})(background {})(details {}))\n".format(
        location, detailed_location, foreground, background, details)
    writefact(fact)

# (deftemplate mood
#  (slot typeof))


def mood():
    mood = input(
        "Enter a mood: ")
    fact = "(mood (typeof {}}))\n".format(mood)
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
# (slot time))

# (deftemplate influence-person
# (slot firstname)
# (slot lastname)
# (slot typeof))

# (deftemplate influence-other
# (slot typeof)
# (multislot comments))


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
    "q": quit
}


def main():
    print("Select a fact template to enter:")
    print("1: person")
    print("2: general location")
    print("3: detailed_location")
    print("4: mood")
    print("5: mood_strength")
    print("q: for quit")
    template_choice = input()
    choose(template_choice)


main()
