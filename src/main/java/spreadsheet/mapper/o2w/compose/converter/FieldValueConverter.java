package spreadsheet.mapper.o2w.compose.converter;

import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * field value extract to human readable value
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface FieldValueConverter<T> extends ValueConverter<T> {

  /**
   * @return which field this converter matched
   * @see FieldMeta#getName()
   */
  String getMatchField();
}
