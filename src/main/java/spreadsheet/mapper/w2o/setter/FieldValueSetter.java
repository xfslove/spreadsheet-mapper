package spreadsheet.mapper.w2o.setter;


import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * object field value setter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface FieldValueSetter<T> extends ValueSetter<T> {

  /**
   * @return which field this setter matched
   * @see FieldMeta#getName()
   */
  String getMatchField();

}
