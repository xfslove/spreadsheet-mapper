package spreadsheet.mapper.w2o.validation.validator.cell;


import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

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
    this(regex, matchField, errorMessage, null);
  }

  public RegexFormatValidator(String regex, String matchField, String errorMessage, String[] dependsOn) {
    this(regex, matchField, matchField, errorMessage, dependsOn);
  }

  public RegexFormatValidator(String regex, String group, String matchField, String errorMessage, String[] dependsOn) {
    this(regex, group, matchField, errorMessage, matchField, dependsOn);
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
