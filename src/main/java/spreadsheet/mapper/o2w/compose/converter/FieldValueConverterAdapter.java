package spreadsheet.mapper.o2w.compose.converter;

import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * field value converter adapter, easy implements customer value converter extends this.
 * <p>
 * Created by hanwen on 5/3/16.
 */
public abstract class FieldValueConverterAdapter<T> implements FieldValueConverter<T> {

  private String matchField;

  public FieldValueConverterAdapter(String matchField) {
    this.matchField = matchField;
  }

  @Override
  public String getMatchField() {
    return matchField;
  }

  @Override
  public abstract String getStringValue(T object, FieldMeta fieldMeta);
}
