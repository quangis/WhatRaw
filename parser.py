#!/usr/bin/env python

#from rdflib import *
#from rdflib.namespace import Namespace, NamespaceManager

from owlready2 import *
import owlready2
owlready2.JAVA_EXE = "C:\\Program Files (x86)\\Java\\jre1.8.0_221\\bin\\java.exe"
import re
import csv

def prulars_to_singular(my_str):
    my_str = re.sub('ies$', 'y', my_str)
    my_str = re.sub('s$', '', my_str)
    return my_str

def what_raw_questions():
    # [SC] load the ontology schema
    onto_path.append("D:/Utrecht/knowledgestructures")
    onto = get_ontology("SpatialQuestionPatternOntology.owl")
    onto.load()

    corpusId = "QuAnCorpus"
    corpusLabel = "QuAnGIS corpus of geo-analytic questions"
    corpusNS = "qac"

    nonobjects = ('and', ',', '-', 'a', 'the', ')', '(')

    subExpress = "\S+"
    subMatcher = re.compile(subExpress, re.IGNORECASE)

    with onto:
        with open('Question corpus_20191015_v0.3.txt', 'r') as datacsvfile:
            datacsvreader = csv.DictReader(datacsvfile, delimiter=';')

            with open('what_raw_intents.csv', 'w', newline='') as intentcsvfile:
                intentfieldnames = ['intent', 'qid', 'code']
                intentwriter = csv.DictWriter(intentcsvfile, fieldnames=intentfieldnames)
                intentwriter.writeheader()

                with open('what_raw_objects.csv', 'w', newline='') as objcsvfile:
                    objfieldnames = ['intent', 'relation', 'object', 'distance', 'qid', 'code']
                    objwriter = csv.DictWriter(objcsvfile, fieldnames=objfieldnames)
                    objwriter.writeheader()

                    with open('what_raw_adjectives.csv', 'w', newline='') as adjcsvfile:
                        adjfieldnames = ['intent', 'adjective', 'distance', 'qid', 'code']
                        adjwriter = csv.DictWriter(adjcsvfile, fieldnames=adjfieldnames)
                        adjwriter.writeheader()

                        with open('what_raw_extents.csv', 'w', newline='') as extentcsvfile:
                            extentfieldnames = ['relation', 'extent', 'qid']
                            extentwriter = csv.DictWriter(extentcsvfile, fieldnames=extentfieldnames)
                            extentwriter.writeheader()

                            intentExpr = (
                                "What "  # required wh start (case insensitive)
                                + "(is|are|were|was|do|does|did|have|has|should|could|would|will) (be )?"  # required auxiliary
                                + "(the |a )?"  # optional article
                                + "(?P<adjective>(.*?))"  # lazy matching any zero or more chars
                                + "(?P<intent>\S+)"  # any non-white space char
                                + "(?P<rightside> (across|along|among|around|at|based on|based upon|between|by|for|from|given|if|in|inside|of|on|over|per|since|that|to|with|within) (.+))"
                            )
                            intentMatcher = re.compile(intentExpr, re.IGNORECASE)
                            #print(intentExpr)

                            extentExpr = (
                                "(?P<objectphrase>.*?)"
                                + "(?P<relation> (across|along|among|around|at|between|by|for|from|in|inside|of|on|per|to|within)) "
                                + "(?P<extent>((?! (across|along|among|around|at|between|by|for|from|in|inside|of|on|per|to|within) ).)*)$"
                            )
                            extentMatcher = re.compile(extentExpr, re.IGNORECASE)
                            #print(extentExpr)

                            simpleObjExp = (
                                "(?P<relation>(across|along|among|around|at|based on|based upon|between|by|for|from|given|in|inside|of|on|over|per|since|that|to|with|within) )"
                                + "("
                                + "(?P<object>(.*?))"  # lazy matching any zero or more chars
                                + "("
                                +   "(?= (across|along|among|around|at|between|by|for|from|given|if|in|inside|like|of|on|over|per|such|that|to|when|where|which|with|within) )"  # positive lookahead
                                +   "|$"
                                + ")"
                                + ")?"  # the entire pattern should occur 0 or 1 time
                            )
                            simpleObjMatcher = re.compile(simpleObjExp, re.IGNORECASE)
                            #print(simpleObjExp)

                            # [TODO] complex object extents: between, of-on, to-for, for-to
                            # [TODO] how to parse sentences that contain if

                            # [SC] create Corpus individual for the ontology
                            corpusIndiv = onto.Corpus(corpusId)
                            corpusIndiv.label = corpusLabel

                            counter = 0
                            qidStr = "c("
                            for q in datacsvreader:
                                intentResult = intentMatcher.search(q['Question'])
                                if intentResult:
                                    print(f"Question {q['ID']}: {q['Question']}")
                                    print(f"Adjective: {intentResult.group('adjective')}")
                                    print(f"Intent: {intentResult.group('intent')}")
                                    #print(f"Right side: {intentResult.group('rightside')}")

                                    intentwriter.writerow({
                                        'intent': intentResult.group('intent')
                                        , 'qid': q['ID']
                                        , 'code': 'o'
                                    })

                                    # [SC] create Question individual
                                    questionIndiv = onto.Question(f"{corpusNS}-{q['ID']}")
                                    questionIndiv.label = q['Question']
                                    # [SC] connect Question and Corpus
                                    corpusIndiv.hasQuestion.append(questionIndiv)

                                    # [SC] create IntentPhrase individual for the ontology
                                    intentPhraseIndiv = onto.IntentPhrase(f"{corpusNS}-{q['ID']}-{intentResult.group('adjective')}{intentResult.group('intent')}")
                                    intentPhraseIndiv.label = f"{intentResult.group('adjective')}{intentResult.group('intent')}"
                                    # [SC] connect IntentPhrase and Question
                                    questionIndiv.hasPhrase.append(intentPhraseIndiv)

                                    # [SC] create Intent individual for the ontology; no need to check for duplicates
                                    intentStr = prulars_to_singular(intentResult.group('intent'))
                                    intentIndiv = onto.Intent(f"i-{intentStr}")
                                    intentIndiv.label = intentStr
                                    # [SC] connect IntentPhrase and Intent
                                    intentPhraseIndiv.hasIntent.append(intentIndiv)

                                    if intentResult.group('adjective'):
                                        # [SC] write to file the entire adjective phrase at first
                                        adjwriter.writerow({
                                            'intent': intentResult.group('intent')
                                            , 'adjective': intentResult.group('adjective')
                                            , 'distance': 0
                                            , 'qid': q['ID']
                                            , 'code': 'o'
                                        })

                                        # [SC] extract and save to a file individual adjective words from the phrase
                                        subResults = subMatcher.findall(intentResult.group('adjective'))
                                        for adjCount in range(len(subResults)):
                                            if subResults[adjCount] not in nonobjects:
                                                adjwriter.writerow({
                                                    'intent': intentResult.group('intent')
                                                    , 'adjective': subResults[adjCount]
                                                    , 'distance': len(subResults) - adjCount
                                                    , 'qid': q['ID']
                                                    , 'code': 'o'
                                                })

                                                # [SC] create Adjective individual for the ontology
                                                adjStr = prulars_to_singular(subResults[adjCount])
                                                adjIndiv = onto.Adjective(f"a-{adjStr}")
                                                adjIndiv.label = adjStr
                                                # [SC] connect Intent and Adjective
                                                intentIndiv.modifiedBy.append(adjIndiv)
                                                # [SC] connect IntentPhrase and Adjective
                                                intentPhraseIndiv.hasWord.append(adjIndiv)

                                    extentResult = extentMatcher.search(intentResult.group('rightside'))
                                    if extentResult:
                                        extentwriter.writerow({
                                            'relation': extentResult.group('relation').strip()
                                            , 'extent': extentResult.group('extent').strip()
                                            , 'qid': q['ID']
                                        })

                                        if extentResult.group('objectphrase'):
                                            simpleObjResult = simpleObjMatcher.search(extentResult.group('objectphrase'))
                                            #print(f"Object phrase: {extentResult.group('objectphrase')}")
                                            print(f"Object relation: {simpleObjResult.group('relation')}")
                                            print(f"Object: {simpleObjResult.group('object')}")

                                            # [SC] write to file the entire object phrase at first
                                            objwriter.writerow({
                                                'intent': intentResult.group('intent')
                                                , 'relation': simpleObjResult.group('relation').replace(' ', '')
                                                , 'object': simpleObjResult.group('object')
                                                , 'distance': 0
                                                , 'qid': q['ID']
                                                , 'code': 'o'
                                            })

                                            # [SC] create IntentPhrase individual for the ontology
                                            objPhraseIndiv = onto.ObjectPhrase(f"{corpusNS}-{q['ID']}-{simpleObjResult.group('relation')}{simpleObjResult.group('object')}")
                                            objPhraseIndiv.label = f"{simpleObjResult.group('relation')}{simpleObjResult.group('object')}"
                                            # [SC] connect IntentPhrase and ObjectPhrase
                                            intentPhraseIndiv.before.append(objPhraseIndiv)
                                            # [SC] connect ObjectPhrase and Question
                                            questionIndiv.hasPhrase.append(objPhraseIndiv)

                                            # [SC] create ObjectRelation individual
                                            objRelationIndiv = onto.ObjectRelation(simpleObjResult.group('relation').strip())
                                            objRelationIndiv.label = simpleObjResult.group('relation').strip()
                                            # [SC] connect ObjectPhrase and ObjectRelation
                                            objPhraseIndiv.hasRelation.append(objRelationIndiv)
                                            # [SC] connect Intent and ObjectRelation
                                            intentIndiv.followedBy.append(objRelationIndiv)

                                            objIndiv = None

                                            # [SC] extract and save to a file individual object words from the phrase
                                            subResults = subMatcher.findall(simpleObjResult.group('object'))
                                            for objectCount in reversed(range(len(subResults))):
                                                if subResults[objectCount] not in nonobjects:
                                                    objwriter.writerow({
                                                        'intent': intentResult.group('intent')
                                                        , 'relation': simpleObjResult.group('relation').replace(' ', '')
                                                        , 'object': subResults[objectCount]
                                                        , 'distance': len(subResults) - objectCount
                                                        , 'qid': q['ID']
                                                        , 'code': 'o'
                                                    })

                                                    if (len(subResults) - objectCount == 1):
                                                        # [SC] create Object individual
                                                        objStr = prulars_to_singular(subResults[objectCount])
                                                        objIndiv = onto.Object(f"o-{objStr}")
                                                        objIndiv.label = objStr
                                                        # [SC] connect ObjectPhrase and Object
                                                        objPhraseIndiv.hasObject.append(objIndiv)
                                                        # [SC] connect ObjectRelation and Object
                                                        objIndiv.precededBy.append(objRelationIndiv)
                                                        # [SC] connect Intent and Object
                                                        intentIndiv.targets.append(objIndiv)
                                                    else:
                                                        # [SC] create Adjective individual for the ontology
                                                        adjStr = prulars_to_singular(subResults[objectCount])
                                                        adjIndiv = onto.Adjective(f"a-{adjStr}")
                                                        adjIndiv.label = adjStr
                                                        # [SC] connect Object and Adjective
                                                        objIndiv.modifiedBy.append(adjIndiv)
                                                        # [SC] connect ObjectPhrase and Adjective
                                                        objPhraseIndiv.hasWord.append(adjIndiv)

                                        print(f"Extent relation: {extentResult.group('relation')}")
                                        print(f"Extent: {extentResult.group('extent')}")
                                        #print(f"Obj phrase: {extentResult.group('objectphrase')}")

                                    #print(f"Relation: {result.group('relation')}")

                                    qidStr += f"{q['ID']},"
                                    counter += 1
                                    print("")
                            print(counter)
                            print(qidStr)

        #sync_reasoner_pellet(onto, infer_property_values = True, infer_data_property_values = True, keep_tmp_file = False)
        #list(default_world.inconsistent_classes())
        onto.save("D:/Utrecht/knowledgestructures/SpatialQuestionPatternOntology.owl")

if __name__ == '__main__':
    what_raw_questions()
