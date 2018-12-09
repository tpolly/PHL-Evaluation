#!/usr/bin/env ruby
require './common.rb'
require 'ap'

recognize_template = {}
[*0..11,13].map(&:to_s).each do |subset|
	recognize_template[subset] = {}
	(0..19).map(&:to_s).each do |i|
		recognize_template[subset][i] = {'digit' => nil, 'displayed_pattern' => Array, 'user_answer' => nil, 'correct' => nil, 'pattern_distance' => nil, 'time_elapsed' => Float, 'repetitions' => Fixnum}
	end
end

construct_template = {}
[*0..11,13].map(&:to_s).each do |subset|
	construct_template[subset] = {}
	(0..9).map(&:to_s).each do |i|
		construct_template[subset][i] = {'digit' => Fixnum, 'correct_answer' => Array, 'user_answer' => Array, 'correct' => nil, 'distance' => nil, 'time_elapsed' => Float, 'repetitions' => Fixnum}
	end
end

common_test_results = {
	'armband_test_wrongs_0' => Fixnum,
	'armband_test_wrongs_1' => Fixnum,
	'recognize' => recognize_template,
	'construct' => construct_template
}

common = {
	'distraction' => DISTRACTIONS,
	'pattern' => {'0' => Array, '1' => Array, '2' => Array, '3' => Array, '4' => Array, '5' => Array, '6' => Array, '7' => Array, '8' => Array, '9' => Array },
	'survey_pre_results' => {'birth_year' => Fixnum, 'gender' => GENDERS},
	'emailadress' => String,
	'survey_post_retest_results' => POST_RETEST_QUESTIONS.map {|i| [i, Fixnum]}.to_h.merge({'comment' => String})
}

template = {
	'NoDistraction' => common.merge({
		'test_results' => common_test_results,
		'survey_post_results' => NO_DISTRACTION_QUESTIONS.map {|i| [i, Fixnum]}.to_h.merge({'comment' => String})
	}),
	
	'LowDistraction' => common.merge({
		'test_results' => common_test_results.merge({
			'distraction' => {
				'gweled_score' => [*-1..11,*100..102].map {|i| [i.to_s, Fixnum]}.to_h
			}
		}),
		'survey_post_results' => LOW_DISTRACTION_QUESTIONS.map {|i| [i, Fixnum]}.to_h.merge({'comment' => String})
	}),
	
	'HighDistraction' => common.merge({
		'test_results' => common_test_results.merge({
			'distraction' => {
				'open_hexagon_score' => [*-1..11,*100..102].map {|i| [i.to_s, Fixnum]}.to_h
			}
		}),
		'survey_post_results' => HIGH_DISTRACTION_QUESTIONS.map {|i| [i, Fixnum]}.to_h.merge({'comment' => String})
	})
}

def checkHash template, hash, before=''
	return nil if template.nil?
	return nil if template == hash
	return nil if Class === template && template === hash
	return nil if Array === template && template.include?(hash)
	if Hash === hash && Hash === template
		msg = []
		template.each do |key, value|
			if hash.keys.include?(key)
				msg.push checkHash(value, hash[key], before + '/' + key)
			else
				msg.push before.to_s + ' - missing key: ' + key.to_s
			end
		end
		return msg.select {|i| !i.nil? && i != []}.flatten
	else
		return before.to_s + ' - no handler:  ' + hash.class
	end
end


RESULT_FILES_WITH_RETEST.each do |file|
	hash = JSON.parse(File.read(file))
	result = checkHash(template[hash['distraction']], hash)
	if result != []
		puts file
		puts result.inspect
	end
end
