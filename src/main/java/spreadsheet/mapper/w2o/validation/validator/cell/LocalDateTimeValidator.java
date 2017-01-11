package spreadsheet.mapper.w2o.validation.validator.cell;

import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * local date time validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateTimeValidator extends CellValidatorAdapter {

  private String pattern;

  public LocalDateTimeValidator(String pattern, String matchField, String errorMessage) {
    this(pattern, matchField, errorMessage, null);
  }

  public LocalDateTimeValidator(String pattern, String matchField, String errorMessage, String[] dependsOn) {
    this(pattern, matchField, matchField, errorMessage, dependsOn);
  }

  public LocalDateTimeValidator(String pattern, String group, String matchField, String errorMessage, String[] dependsOn) {
    this(pattern, group, matchField, errorMessage, matchField, dependsOn);
  }

  public LocalDateTimeValidator(String pattern, String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
    this.pattern = pattern;
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(pattern);

    try {
      dateTimeFormatter.parseLocalDateTime(cell.getValue());
    } catch (IllegalArgumentException e) {
      return false;
    }
    return true;
  }
}
