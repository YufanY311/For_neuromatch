#===================== DO NOT EDIT THIS BLOCK =================================#
import collections
from collections import namedtuple
import json
import os

TEXT_FORMAT = "text"  # question type when expected answer is a str, int, float, or bool
NO_RERUN = "no_rerun" #any question involving paths. Do not expect the rerun to match submission
TEXT_NO_RERUN = TEXT_FORMAT + " " + NO_RERUN
TEXT_FORMAT_NAMEDTUPLE = "text namedtuple"  # question type when expected answer is a namedtuple
TEXT_FORMAT_UNORDERED_LIST = "text list_unordered"  # question type when the expected answer is a list where the order does *not* matter
TEXT_FORMAT_ORDERED_LIST = "text list_ordered"  # question type when the expected answer is a list where the order does matter
TEXT_FORMAT_ORDERED_LIST_NO_RERUN = "text list_ordered no_rerun"  # question type when the expected answer is a list where the order does matter and contains paths
TEXT_FORMAT_ORDERED_LIST_NAMEDTUPLE = "text list_ordered namedtuple"  # question type when the expected answer is a list of namedtuples where the order does matter
TEXT_FORMAT_SPECIAL_ORDERED_LIST = "text list_special_ordered"  # question type when the expected answer is a list where order does matter, but with possible ties. All tied elements are put in a list, where internal order does not matter.
TEXT_FORMAT_DICT = "text dict"  # question type when the expected answer is a dictionary
TEXT_FORMAT_LIST_DICTS_ORDERED = "text list_dicts_ordered"  # question type when the expected answer is a list of dicts where the order does matter
PNG_FORMAT = "png"  # use when the expected answer is an image
HTML_FORMAT = "html"
FILE_FORMAT = "file"
FILE_JSON_FORMAT = "file json"

Question = collections.namedtuple("Question", ["number", "weight", "format"])
#===================== DO NOT EDIT THIS BLOCK =================================#


REQUIRED_FILES = ['expected.html', 'institutions.json', 'lint.py']

# set this to [] if they're not supposed to download any files
FILES_TO_DOWNLOAD = ['2019-2020.html', '2020-2021.html', '2021-2022.html', 'rankings.json']


questions = [
    Question(number=1, weight=1, format=TEXT_FORMAT),
    Question(number=2, weight=1, format=HTML_FORMAT),
    Question(number=3, weight=1, format=HTML_FORMAT),
    Question(number=4, weight=1, format=TEXT_FORMAT),
    Question(number=5, weight=1, format=TEXT_FORMAT),
    Question(number=6, weight=1, format=HTML_FORMAT),
    Question(number=7, weight=1, format=TEXT_FORMAT),
    Question(number=8, weight=1, format=TEXT_FORMAT_UNORDERED_LIST),
    Question(number=9, weight=1, format=TEXT_FORMAT),
    Question(number=10, weight=1, format=TEXT_FORMAT_ORDERED_LIST),
    Question(number=11, weight=1, format=TEXT_FORMAT),
    #Question(number=11.5, weight=0, format=FILE_JSON_FORMAT), #optional
    Question(number=12, weight=1, format=TEXT_FORMAT_UNORDERED_LIST),
    Question(number=13, weight=1, format=TEXT_FORMAT),
    Question(number=14, weight=1, format=TEXT_FORMAT),
    Question(number=15, weight=1, format=TEXT_FORMAT_UNORDERED_LIST),
    Question(number=16, weight=1, format=TEXT_FORMAT_UNORDERED_LIST),
    Question(number=17, weight=1, format=TEXT_FORMAT_UNORDERED_LIST),
    Question(number=18, weight=1, format=TEXT_FORMAT_ORDERED_LIST),
    Question(number=19, weight=1, format=TEXT_FORMAT_LIST_DICTS_ORDERED),
    Question(number=20, weight=1, format=FILE_JSON_FORMAT)
]




expected_json = {
    "1": 103,
    "2": ['Harvard University', 'Harvard University', 'Harvard University'],
    "4": 19,
    "5": 87.26666666666667,
    "7": 'Ludwig Maximilian University of Munich',
    "8": ['Harvard University',
            'Massachusetts Institute of Technology',
            'Stanford University',
            'Columbia University',
            'Princeton University',
            'University of California, Berkeley',
            'University of Pennsylvania',
            'University of Chicago',
            'California Institute of Technology',
            'Yale University',
            'Cornell University',
            'Northwestern University',
            'University of California, Los Angeles',
            'University of Michigan, Ann Arbor',
            'Johns Hopkins University',
            'University of Washington - Seattle',
            'University of Illinois at Urbana–Champaign',
            'Duke University',
            'University of Wisconsin–Madison',
            'New York University',
            'University of California San Diego',
            'University of Texas at Austin',
            'University of California, San Francisco',
            'University of North Carolina at Chapel Hill',
            'University of Minnesota - Twin Cities',
            'University of Texas Southwestern Medical Center',
            'Washington University in St. Louis',
            'University of Southern California',
            'Brown University',
            'Vanderbilt University',
            'Pennsylvania State University',
            'Rutgers University–New Brunswick',
            'Dartmouth College',
            'University of California, Davis'],
    "9": 'Fudan University',
    "10": ['Indian Institute of Science',
             'Tata Institute of Fundamental Research',
             'Indian Institute of Technology Bombay',
             'University of Delhi',
             'Indian Institute of Technology Madras'],
    "11": 1856,
    #"11.5": {'institutions.json': 'my_institutions.json'},
    "12": ['Academy of Scientific & Innovative Research',
            'International Institute for Management Development',
            'Tôn Đức Thắng University',
            'SOAS University of London',
            'Antioch College',
            'École nationale supérieure de chimie de Montpellier',
            'USI - University of Italian Speaking Switzerland',
            'Federal University of Mato Grosso do Sul',
            'Haverford College'],
    "13": 'USI - University of Italian Speaking Switzerland',
    "14": 451,
    "15": ["École nationale d'administration",
             'INSEAD',
             'HEC Paris',
             'Institut Polytechnique de Paris',
             'University of Tokyo',
             'International Institute for Management Development',
             'China Europe International Business School'],
    "16": ['École Polytechnique Fédérale de Lausanne',
             'Emory University',
             'University of Utah',
             'École Polytechnique',
             'University of Texas MD Anderson Cancer Center',
             'University of Groningen',
             'Tufts University',
             'Paris-Sud University',
             'Paris Diderot University',
             'University of California San Diego',
             'Aarhus University',
             'École normale supérieure'],
    "17": ['USA', 'United Kingdom'],
    "18": ['World Rank',
            'Institution',
            'Country',
            'National Rank',
            'Quality of Education Rank',
            'Alumni Employment Rank',
            'Quality of Faculty Rank',
            'Research Performance Rank',
            'Score'],
    "19": [{'World Rank': 1,
              'Year': '2019-2020',
              'Institution': 'Harvard University',
              'Country': 'USA',
              'National Rank': 1,
              'Quality of Education Rank': 2,
              'Alumni Employment Rank': 1,
              'Quality of Faculty Rank': 1,
              'Research Performance Rank': 1,
              'Score': 100.0},
             {'World Rank': 2,
              'Year': '2019-2020',
              'Institution': 'Massachusetts Institute of Technology',
              'Country': 'USA',
              'National Rank': 2,
              'Quality of Education Rank': 1,
              'Alumni Employment Rank': 10,
              'Quality of Faculty Rank': 2,
              'Research Performance Rank': 5,
              'Score': 96.7},
             {'World Rank': 3,
              'Year': '2019-2020',
              'Institution': 'Stanford University',
              'Country': 'USA',
              'National Rank': 3,
              'Quality of Education Rank': 9,
              'Alumni Employment Rank': 3,
              'Quality of Faculty Rank': 3,
              'Research Performance Rank': 2,
              'Score': 95.2},
             {'World Rank': 4,
              'Year': '2019-2020',
              'Institution': 'University of Cambridge',
              'Country': 'United Kingdom',
              'National Rank': 1,
              'Quality of Education Rank': 4,
              'Alumni Employment Rank': 19,
              'Quality of Faculty Rank': 5,
              'Research Performance Rank': 11,
              'Score': 94.1},
             {'World Rank': 5,
              'Year': '2019-2020',
              'Institution': 'University of Oxford',
              'Country': 'United Kingdom',
              'National Rank': 2,
              'Quality of Education Rank': 10,
              'Alumni Employment Rank': 24,
              'Quality of Faculty Rank': 10,
              'Research Performance Rank': 4,
              'Score': 93.3}],
    "20": {'rankings.json': 'my_rankings.json'}
}
