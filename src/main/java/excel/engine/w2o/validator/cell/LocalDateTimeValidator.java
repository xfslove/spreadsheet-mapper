package excel.engine.w2o.validator.cell;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * local date time validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateTimeValidator extends CellValidatorAdapter {

  private String format;

  public LocalDateTimeValidator(String matchField, String format, String errorMessage) {
    super(matchField, errorMessage);
    this.format = format;
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(format);

    try {
      dateTimeFormatter.parseLocalDateTime(cell.getValue());
    } catch (IllegalArgumentException e) {
      return false;
    }
    return true;
  }
}
