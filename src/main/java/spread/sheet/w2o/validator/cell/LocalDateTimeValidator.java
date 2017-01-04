package spread.sheet.w2o.validator.cell;

import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * local date time validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateTimeValidator extends CellValidatorAdapter {

  private String format;

  public LocalDateTimeValidator(String format, String matchField, String errorMessage) {
    super(matchField, errorMessage);
    this.format = format;
  }

  public LocalDateTimeValidator(String format, String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
    this.format = format;
  }

  public LocalDateTimeValidator(String format, String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
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
