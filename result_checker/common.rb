#!/usr/bin/env ruby
require 'json'
require 'awesome_print'
PATH = File.expand_path(File.dirname(__FILE__))

DISTRACTIONS = ['LowDistraction', 'HighDistraction', 'NoDistraction']

GENDERS = ['männlich', 'weiblich', 'keine Angabe']

COMMON_QUESTIONS = [
	'Die Vibration war zu stark.',
	'Die Vibration war zu schwach.',
	'Die Vibration war störend/unangenehm.',
	'Lange und kurze Vibrationen waren gut zu unterscheiden.',
	'Ich konnte das Summen der Vibrationsmotoren hören.',
	'Das Summen der Motoren zu Hören hat mir bei der Wahrnehmung der Muster geholfen.',
	'Ich war motiviert, die Muster bestmöglichst zu lernen.',
	'Ich konnte im Test die Muster gut erkennen.',
	'Ich konnte im Test die Muster gut wiedergeben.',
]

COMMON_DISTRACTION_QUESTIONS = [
	'Ich kannte das Spiel bereits gut',
	'Ich habe mich während des Spiels auf die Vibrationsmuster konzentriert.',
	'Ich habe mich auf das Spiel konzentriert.',
	'Die Vibration hat vom Spiel abgelenkt.',
	'Die Zahlwörter haben vom Spiel abgelenkt.',
	'Das Spiel hat meine volle Aufmerksamkeit erfordert.',
	'Das Spiel hat mir Spaß gemacht.',
	'Ich war motiviert, das Spiel bestmöglichst zu spielen.',
]

NO_DISTRACTION_QUESTIONS = COMMON_QUESTIONS + ['Die Muster folgten zu schnell aufeinander.', 'Die Muster folgten zu langsam aufeinander.', 'Ich konnte mir die Muster leicht merken.', 'Ich habe Eselsbrücken verwendet.']

LOW_DISTRACTION_QUESTIONS = COMMON_QUESTIONS + COMMON_DISTRACTION_QUESTIONS
HIGH_DISTRACTION_QUESTIONS = LOW_DISTRACTION_QUESTIONS

POST_RETEST_QUESTIONS = [
	'Ich konnte mich gut an die Muster erinnern.',
	'Die Auflösung war für mich wenig überraschend.',
]

RESULT_FILES_WITH_RETEST = Dir.glob(PATH + "/../Results/with-retest/*.json")
RESULT_FILES_NO_RETEST = Dir.glob(PATH + "/../Results/no-retest/*.json")
RESULT_FILES_TODO_RETEST = Dir.glob(PATH + "/../Results/todo-retest/*.json")
RESULT_FILES_ALL = RESULT_FILES_WITH_RETEST + RESULT_FILES_NO_RETEST + RESULT_FILES_TODO_RETEST
