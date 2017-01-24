package spreadsheet.mapper.w2o.validation.validator.cell.buildin;

import spreadsheet.mapper.model.core.Cell;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.w2o.param.BooleanParam;
import spreadsheet.mapper.w2o.validation.validator.cell.CustomSingleCellValidatorAdapter;

/**
 * boolean validator
 * <p>
 * Created by hanwen on 2017/1/11.
 */
public class BooleanValidator extends CustomSingleCellValidatorAdapter<BooleanValidator> {

  private BooleanParam param;

  public BooleanValidator param(BooleanParam param) {
    this.param = param;
    return this;
  }

  @Override
  protected BooleanValidator getThis() {
    return this;
  }

  @Override
  protected boolean customValid(Cell cell, FieldMeta fieldMeta) {
    String value = cell.getValue();
    return param.getSupportedTrue().contains(value) || param.getSupportedFalse().contains(value);
  }
}
