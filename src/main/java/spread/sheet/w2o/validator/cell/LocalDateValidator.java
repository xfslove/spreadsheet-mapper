package spread.sheet.w2o.validator.cell;

import spread.sheet.model.core.Cell;
import spread.sheet.model.meta.FieldMeta;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

/**
 * local date validator
 * <p>
 * Created by hanwen on 5/3/16.
 */
public class LocalDateValidator extends CellValidatorAdapter {

  private String pattern;

  public LocalDateValidator(String pattern, String matchField, String errorMessage) {
    super(matchField, errorMessage);
    this.pattern = pattern;
  }

  public LocalDateValidator(String pattern, String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
    this.pattern = pattern;
  }

  public LocalDateValidator(String pattern, String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
    this.pattern = pattern;
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {

    DateTimeFormatter dateTimeFormatter = DateTimeFormat.forPattern(pattern);

    try {
      dateTimeFormatter.parseLocalDate(cell.getValue());
    } catch (IllegalArgumentException e) {
      return false;
    }
    return true;
  }
}
