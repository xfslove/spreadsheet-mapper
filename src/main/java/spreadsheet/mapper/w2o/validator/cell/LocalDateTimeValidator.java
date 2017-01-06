package spreadsheet.mapper.w2o.validator.cell;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * local date time validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateTimeValidator extends CellValidatorAdapter {

  private String pattern;

  public LocalDateTimeValidator(String pattern, String matchField, String errorMessage) {
    super(matchField, errorMessage);
    this.pattern = pattern;
  }

  public LocalDateTimeValidator(String pattern, String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
    this.pattern = pattern;
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
