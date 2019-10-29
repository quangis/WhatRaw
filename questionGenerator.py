from numpy.random import choice
import pandas as pd

def generateQuestion():
    question = "What is the "
    objPhrase = ""
    intentPhrase = ""

    intentWord = choice(intentStatePDF.intent, 1, p=intentStatePDF.IntentP)[0]
    print(f"Intent: {intentWord}")

    intentState = choice(intentStates, 1, p=intentStatePDF.loc[intentStatePDF['intent'] == intentWord][intentStates].iloc[0])[0]
    print(f"Intent state: {intentState}")

    # [SC] pick object with a relation and one or more adjectives
    if (intentState == 'SoloObjP' or intentState == 'AdjObjP'):
        # [SC] pick object
        objectWord = choice(intentObjCoocDF.index.values, 1, p=intentObjCoocDF[intentWord])[0]
        print(f"Object: {objectWord}")

        # [SC] pick relation
        relationWord = choice(intentRelationCoocDF.index.values, 1, p=intentRelationCoocDF[intentWord])[0]
        print(f"Object: {relationWord}")

        # [SC] pick object adjective
        if objectWord in objAdjCoocDF.columns:
            objAdjWord = choice(objAdjCoocDF.index.values, 1, p=objAdjCoocDF[objectWord])[0]
            print(f"Object adjective: {objAdjWord}")
            objPhrase = f" {relationWord} {objAdjWord} {objectWord}"
        else:
            objPhrase = f" {relationWord} {objectWord}"

    # [SC] pick one or more adjectives for the intent
    if (intentState == 'SoloAdjP' or intentState == 'AdjObjP'):
        intentAdjWord = choice(intentAdjCoocDF.index.values, 1, p=intentAdjCoocDF[intentWord])[0]
        print("Picking adjectives")
        intentPhrase = f"{intentAdjWord} {intentWord}"
    else:
        intentPhrase = f"{intentWord}"

    extentRelation = choice(extentDF.relation, 1)[0]
    extentWord = choice(extentDF.extent, 1)[0]

    question += f"{intentPhrase}{objPhrase} {extentRelation} {extentWord}?"

    return question


if __name__ == '__main__':
    intentStates = ['SoloP', 'SoloAdjP', 'SoloObjP', 'AdjObjP']

    intentStatePDF = pd.read_csv('intentStateP.txt', delimiter=";", header=0, index_col=False)
    intentAdjCoocDF = pd.read_csv('intentAdjCooc.txt', delimiter=";", header=0, index_col=0)
    intentObjCoocDF = pd.read_csv('intentObjCooc.txt', delimiter=";", header=0, index_col=0)
    intentRelationCoocDF = pd.read_csv('intentRelationCooc.txt', delimiter=";", header=0, index_col=0)
    objAdjCoocDF = pd.read_csv('objAdjCooc.txt', delimiter=";", header=0, index_col=0)
    extentDF = pd.read_csv('what_raw_extents.csv', delimiter=",", header=0, index_col=False)

    with open('generated_questions.txt', 'w') as writer:
        for count in range(500):
            question = generateQuestion()
            writer.write(f"{question}\n")
            print(question)