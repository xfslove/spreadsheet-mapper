package spread.sheet.w2o.validator.cell;


import spread.sheet.model.meta.FieldMeta;
import spread.sheet.model.core.Cell;

/**
 * regex format validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class RegexFormatValidator extends CellValidatorAdapter {

  /**
   * regex statement
   */
  private String regex;

  public RegexFormatValidator(String regex, String matchField, String errorMessage) {
    super(matchField, errorMessage);
    this.regex = regex;
  }

  public RegexFormatValidator(String regex, String matchField, String errorMessage, String[] dependsOn) {
    super(matchField, errorMessage, dependsOn);
    this.regex = regex;
  }

  public RegexFormatValidator(String regex, String group, String matchField, String errorMessage, String messageOnField, String[] dependsOn) {
    super(group, matchField, errorMessage, messageOnField, dependsOn);
    this.regex = regex;
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    return cell.getValue().matches(regex);
  }
}
