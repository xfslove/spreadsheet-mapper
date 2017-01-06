package spreadsheet.mapper.w2o.setter;


import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.core.Cell;

/**
 * field value setter adapter, easy implements customer value setter extends this.
 * <p>
 * Created by hanwen on 15-12-16.
 */
public abstract class FieldValueSetterAdapter<T> implements FieldValueSetter<T> {

  private String matchField;

  public FieldValueSetterAdapter(String matchField) {
    this.matchField = matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public abstract void set(T object, Cell cell, FieldMeta fieldMeta);
}
