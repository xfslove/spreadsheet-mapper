package spread.sheet.w2o.setter;


/**
 * object field value setter
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface FieldValueSetter<T> extends ValueSetter<T> {

  /**
   * @return which field this setter matched
   */
  String getMatchField();

}
