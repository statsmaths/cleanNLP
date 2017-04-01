import spacy
import os
import time
import datetime

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
        tfile, dfile, efile, mfile, vfile = prepare_output(self.output_dir)

        id = self.doc_id_offset
        for this_file in input_files:
            with open(this_file, 'r', errors = 'replace') as ifile:
                text = ifile.read().replace('\r\n', ' ').replace('\n', ' ')
            doc = self.nlp(text, tag = True, parse = True, entity = self.entity_flag)

            self.createSentDict(doc)
            save_doc_meta(mfile, id, self.language, this_file)
            save_doc_token(doc, tfile, id)
            save_doc_dependency(doc, dfile, id)
            if self.entity_flag:
                self.save_doc_entity(doc, efile, id)
            if self.vector_flag:
                save_doc_vector(doc, vfile, id)
            id += 1

        tfile.close()
        dfile.close()
        efile.close()

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
            tid_end = ent.end - ent.sent.start + 1
            entity = ent.text.replace('"','')
            orow = '{:d},{:d},{:d},{:d},"{:s}","{:s}",""\n'.format(id, sid,
                    tid_start, tid_end, ent.root.ent_type_, entity)
            _ = efile.write(orow)


def save_doc_meta(mfile, id, language, fname):
    ts = time.time()
    st = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d %H:%M:%S')
    orow = str('{:d},"{:s}","{:s}","{:s}","{:s}"\n').format(id, st, spacy.about.__version__, language, fname)
    _ = mfile.write(orow)


def save_doc_token(doc, tfile, id):
    sid = 0
    tid = 0
    for x in doc.sents:
        tid = 0
        # The 0th token is always the ROOT
        orow = str('{:d},{:d},{:d},"ROOT","ROOT","","","","",,\n').format(id, sid, tid)
        _ = tfile.write(orow)

        # Now, parse the actual tokens, starting at 1
        tid = 1
        for word in x:
            this_text = word.text
            this_lemma = word.lemma_
            if this_text == '\"':
                this_text = '\\\''
                this_lemma = '\\\''
            if this_text == "'":
                this_test = '\\\''
                this_lemma = '\\\''
            orow = str('{:d},{:d},{:d},"{:s}","{:s}","{:s}","{:s}","","",{:d},{:d}\n').format(id, sid,
                    tid, this_text, this_lemma, word.pos_, word.tag_,
                    word.idx, word.idx + len(word.text))
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
            orow = '{:d},{:d},{:d},{:d},{:d},"{:s}",""\n'.format(id, sid,
                tid, sid, dep_id, word.dep_)
            _ = dfile.write(orow)
            tid += 1
        sid += 1


def save_doc_vector(doc, vfile, id):
    sid = 0
    tid = 0
    for x in doc.ents:
        tid = 0
        for word in x:
            orow = '{:d},{:d},{:d},'.format(id, sid, tid)
            _ = vfile.write(orow)
            # TODO: How precise does this really need to be? 4 decimal
            #       places seems enough and saves room; is anything
            #       lost?
            word.vector.tofile(vfile, sep = ",", format = "%1.04f")
            _ = vfile.write("\n")
            tid += 1
        sid += 1


def prepare_output(output_dir):
    if output_dir is None:
        raise RuntimeError("You must set the output directory before parsing.")

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    tfile = open(os.path.join(output_dir, "token.csv"), "w")
    dfile = open(os.path.join(output_dir, "dependency.csv"), "w")
    efile = open(os.path.join(output_dir, "entity.csv"), "w")
    mfile = open(os.path.join(output_dir, "document.csv"), "w")
    vfile = open(os.path.join(output_dir, "vector.csv"), "w")

    # Now, add headers (even if they are just empty)
    _ = tfile.write("id,sid,tid,word,lemma,upos,pos,speaker,wiki,cid,cid_end\n")
    _ = dfile.write("id,sid,tid,sid_target,tid_target,relation,relation_full\n")
    _ = efile.write("id,sid,tid,tid_end,entity_type,entity,entity_normalized\n")
    _ = mfile.write("id,time,version,language,uri\n")

    with open(os.path.join(output_dir, "sentiment.csv"), 'w') as temp_file:
        _ = temp_file.write("id,sid,pred_class,p0,p1,p2,p3,p4\n")

    with open(os.path.join(output_dir, "coreference.csv"), 'w') as temp_file:
        _ = temp_file.write("id,rid,mid,mention,mention_type,number,gender,animacy,sid,tid,tid_end,tid_head\n")

    with open(os.path.join(output_dir, "triple.csv"), 'w') as temp_file:
        header = "id,subject,object,relation,confidence,be_prefix,be_suffix,"
        header +=  "of_suffix,tmod,sid,tid_subject,tid_subject_end,tid_object,"
        header += "tid_object_end,tid_relation,tid_relation_end\n"
        _ = temp_file.write(header)

    return tfile, dfile, efile, mfile, vfile

