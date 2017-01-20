package spreadsheet.mapper.o2w.compose.converter;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * field value converter adapter, easy implements customer value converter extends this.
 * <p>
 * Created by hanwen on 5/3/16.
 */
public abstract class FieldConverterAdapter<T, V extends FieldConverterAdapter<T, V>> implements FieldConverter<T> {

  private String matchField;

  public V matchField(String matchField) {
    this.matchField = matchField;
    return getThis();
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  protected abstract V getThis();

  @Override
  public abstract String getValue(T object, Cell cell, FieldMeta fieldMeta);
}
