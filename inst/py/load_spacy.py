import spacy
import os
import datetime
import io
import numpy as np

class SpacyCleanNLP:
    """ A class to call spacy and output normalized tables """
    output_dir = None
    doc_id_offset = 0
    language = ""
    entity_flag = True
    vector_flag = False
    sent_index = {}

    def __init__(self, model_name = 'en'):
        self.nlp = spacy.load(model_name)

    def setOutputPath(self, output_dir):
        self.output_dir = output_dir

    def setLanguage(self, language):
        self.language = language

    def setIdOffset(self, doc_id_offset):
        self.doc_id_offset = doc_id_offset

    def setEntityFlag(self, entity_flag):
        self.entity_flag = entity_flag

    def setVectorFlag(self, vector_flag):
        self.vector_flag = vector_flag

    def processFiles(self, input_files):
        tfile, dfile, efile, mfile, vfile = prepare_output(self.output_dir, self.entity_flag, self.vector_flag)

        id = self.doc_id_offset
        for this_file in input_files:
            with io.open(this_file, 'r', errors = 'replace') as ifile:
                text = ifile.read().replace(u'\r\n', u' ').replace(u'\n', u' ')
            #doc = self.nlp(text, tag = True, parse = True, entity = self.entity_flag)
            doc = self.nlp(text)

            self.createSentDict(doc)
            save_doc_meta(mfile, id, self.language, this_file)
            save_doc_token(doc, tfile, id)
            save_doc_dependency(doc, dfile, id)
            if self.entity_flag:
                self.save_doc_entity(doc, efile, id)
            if self.vector_flag:
                save_doc_vector(doc, vfile, id)
            id += 1

        if tfile != None:
            tfile.close()
        if dfile != None:
            dfile.close()
        if efile != None:
            efile.close()
        if vfile != None:
            vfile.close()

    def createSentDict(self, doc):
        self.sent_index = {}
        sid = 0
        for sent in doc.sents:
            self.sent_index[sent.start] = sid
            sid += 1

    def save_doc_entity(self, doc, efile, id):
        for ent in doc.ents:
            sid = self.sent_index.get(ent.sent.start, -1)
            tid_start = ent.start - ent.sent.start + 1
            # NOTE: spaCy point one beyond entity, so do not add 1 here to tid_end:
            tid_end = ent.end - ent.sent.start
            entity = ent.text.replace('"','')
            orow = u'{:d},{:d},{:d},{:d},"{:s}","{:s}"\n'.format(id, sid + 1,
                    tid_start, tid_end, ent.root.ent_type_, entity)
            _ = efile.write(orow)


def save_doc_meta(mfile, id, language, fname):
    st = datetime.datetime.utcnow().strftime('%Y-%m-%d %H:%M:%SZ')
    orow = u'{:d},"{:s}","{:s}","{:s}","{:s}"\n'.format(id, st, spacy.about.__version__, language, fname)
    _ = mfile.write(orow)


def save_doc_token(doc, tfile, id):
    sid = 0
    tid = 0
    for x in doc.sents:
        tid = 0
        # The 0th token is always the ROOT
        orow = u'{:d},{:d},{:d},"ROOT","ROOT","","",\n'.format(id, sid + 1, tid)
        _ = tfile.write(orow)

        # Now, parse the actual tokens, starting at 1
        tid = 1
        for word in x:
            this_text = word.text
            this_lemma = word.lemma_
            this_text = this_text.replace("\"", "\\\'")
            this_text = this_text.replace("\'", "\\\'")
            this_lemma = this_lemma.replace("\"", "\\\'")
            this_lemma = this_lemma.replace("\'", "\\\'")

            # if this_text == '\"':
            #     this_text = '\\\''
            #     this_lemma = '\\\''
            # if this_text == "'":
            #     this_text = '\\\''
            #     this_lemma = '\\\''
            # if this_lemma == '\"':
            #     this_lemma = '\\\''
            # if this_lemma == "'":
            #     this_lemma = '\\\''

            orow = u'{:d},{:d},{:d},"{:s}","{:s}","{:s}","{:s}",{:d}\n'.format(id, sid + 1,
                    tid, this_text, this_lemma, word.pos_, word.tag_, word.idx)
            _ = tfile.write(orow)
            tid += 1
        sid += 1


def save_doc_dependency(doc, dfile, id):
    sid = 0
    tid = 0
    for x in doc.sents:
        # spacy counts tokens over the whole doc, but we
        # want it within a sentence as with coreNLP. So
        # save the first at sentence start and substract
        # it off
        start_token_i = x[0].i
        tid = 1

        for word in x:
            if word.dep_ == "ROOT":
                dep_id = 0
            else:
                dep_id = word.head.i - start_token_i + 1
            orow = u'{:d},{:d},{:d},{:d},"{:s}",""\n'.format(id, sid + 1,
                dep_id, tid, word.dep_)
            _ = dfile.write(orow)
            tid += 1
        sid += 1


def save_doc_vector(doc, vfile, id):
    sid = 0
    tid = 0
    for x in doc.sents:
        tid = 1 # no vector for the ROOT
        for word in x:
            orow = u'{:d},{:d},{:d},'.format(id, sid + 1, tid)
            _ = vfile.write(orow)
            # TODO: How precise does this really need to be? 4 decimal
            #       places seems enough and saves room; is anything
            #       lost?
            x = np.char.mod('%1.04f', word.vector)
            _ = vfile.write(u",".join(x))
            _ = vfile.write(u"\n")
            tid += 1
        sid += 1


def prepare_output(output_dir, entity_flag, vector_flag):
    if output_dir is None:
        raise RuntimeError("You must set the output directory before parsing.")

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    # token
    tfile = io.open(os.path.join(output_dir, "token.csv"), "w")
    _ = tfile.write(u"id,sid,tid,word,lemma,upos,pos,cid\n")

    # dependency
    dfile = io.open(os.path.join(output_dir, "dependency.csv"), "w")
    _ = dfile.write(u"id,sid,tid,tid_target,relation,relation_full\n")

    # document
    mfile = io.open(os.path.join(output_dir, "document.csv"), "w")
    _ = mfile.write(u"id,time,version,language,uri\n")

    # vector
    if vector_flag:
        vfile = io.open(os.path.join(output_dir, "vector.csv"), "w")
    else:
        vfile = None

    # entity
    if entity_flag:
        efile = io.open(os.path.join(output_dir, "entity.csv"), "w")
        _ = efile.write(u"id,sid,tid,tid_end,entity_type,entity\n")
    else:
        efile = None

    return tfile, dfile, efile, mfile, vfile

