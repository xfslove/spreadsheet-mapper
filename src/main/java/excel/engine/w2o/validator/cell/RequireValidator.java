package excel.engine.w2o.validator.cell;

import excel.engine.model.core.Cell;
import excel.engine.model.meta.FieldMeta;
import org.apache.commons.lang3.StringUtils;

/**
 * required validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class RequireValidator extends CellValidatorAdapter {

  public RequireValidator(String matchField) {
    super(matchField, "应该为必填");
  }

  public RequireValidator(String matchField, String errorMessage) {
    super(matchField, errorMessage);
  }

  @Override
  public boolean valid(Cell cell, FieldMeta fieldMeta) {
    return customValidate(cell, fieldMeta);
  }

  @Override
  protected boolean customValidate(Cell cell, FieldMeta fieldMeta) {
    return StringUtils.isNotBlank(cell.getValue());
  }
}
