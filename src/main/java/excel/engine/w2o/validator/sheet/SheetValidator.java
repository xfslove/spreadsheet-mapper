package excel.engine.w2o.validator.sheet;

import excel.engine.model.core.Sheet;
import excel.engine.model.meta.SheetMeta;
import excel.engine.w2o.validator.Validator;

/**
 * excel sheet validator, after workbook validators, if workbook validators failure, sheet validators will skip.
 * <p>
 * Created by hanwen on 2016/12/23.
 */
public interface SheetValidator extends Validator {

  /**
   * valid supplied excel sheet
   *
   * @param sheet     sheet
   * @param sheetMeta sheet meta
   * @return true if pass
   */
  boolean valid(Sheet sheet, SheetMeta sheetMeta);
}
