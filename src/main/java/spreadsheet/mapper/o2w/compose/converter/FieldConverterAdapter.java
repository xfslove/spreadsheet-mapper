package spreadsheet.mapper.o2w.compose.converter;

import org.apache.commons.lang3.StringUtils;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.validation.WorkbookValidateException;

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
    if (StringUtils.isBlank(matchField)) {
      throw new WorkbookValidateException("field value converter match field must be set");
    }
    return matchField;
  }

  protected abstract V getThis();

  @Override
  public abstract String getValue(T object, Cell cell, FieldMeta fieldMeta);
}
