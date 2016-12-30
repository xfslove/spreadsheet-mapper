package excel.engine.w2o.validator.cell;


import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;

/**
 * regex format validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class RegexFormatValidator extends CellValidatorAdapter {

  /**
   * regex statement
   */
  protected String regex;

  public RegexFormatValidator(String matchField, String regex, String errorMessage) {
    super(matchField, errorMessage);
    this.regex = regex;
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    return cell.getValue().matches(regex);
  }
}
