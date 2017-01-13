package spreadsheet.mapper.w2o.validation.validator.cell;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;

/**
 * regex format validator
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class RegexFormatValidator extends CellValidatorAdapter<RegexFormatValidator> {
  /**
   * regex statement
   */
  private String regex;

  public RegexFormatValidator regex(String regex) {
    this.regex = regex;
    return getThis();
  }

  @Override
  protected RegexFormatValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
    return cell.getValue().matches(getRegex());
  }

  /*=====================
    for customer access
   =====================*/
  protected String getRegex() {
    return regex;
  }
}
