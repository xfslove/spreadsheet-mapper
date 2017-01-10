package spreadsheet.mapper.o2w.extractor;

import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * field value extract to human readable value
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface FieldValueExtractor<T> extends ValueExtractor<T> {

  /**
   * @return which field this extractor matched
   * @see FieldMeta#getName()
   */
  String getMatchField();
}
