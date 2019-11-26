from enum import Enum

###create a feature type class to be used for information extraction
class FeatureType(Enum):
    PAGE = 1
    BLOCK = 2
    PARA = 3
    WORD = 4
    SYMBOL = 5

#document is result of the vision api, feature is one of the options from FeatureType
def get_document_bounds(document, feature):
    bounds=[]
    for i,page in enumerate(document.pages):
        for block in page.blocks:
            if feature==FeatureType.BLOCK:
                bounds.append(block.bounding_box)
            for paragraph in block.paragraphs:
                if feature==FeatureType.PARA:
                    bounds.append(paragraph.bounding_box)
                for word in paragraph.words:
                    for symbol in word.symbols:
                        if (feature == FeatureType.SYMBOL):
                            bounds.append(symbol.bounding_box)
                    if (feature == FeatureType.WORD):
                        bounds.append(word.bounding_box)
    return bounds

##word is the word object of the api
def assemble_word(word):
    assembled_word=""
    for symbol in word.symbols:
        assembled_word+=symbol.text
    return assembled_word

#document is result of the vision api
def get_doc_content(document):
    blocks = []
    for i,page in enumerate(document.pages):
        for block in page.blocks:
            paragraphs = []
            for paragraph in block.paragraphs:
                words = []
                for word in paragraph.words:
                        symbols = []
                        words.append([assemble_word(word), word.confidence, word.property, word.bounding_box])
                paragraphs.append(words)
            blocks.append(paragraphs)
    return blocks

#document is result of the vision api
def get_words_content(document) :
    words = []
    for i,page in enumerate(document.pages):
        for block in page.blocks:
            for paragraph in block.paragraphs:
                for word in paragraph.words:
                        words.append([assemble_word(word), word.confidence])
    return words
