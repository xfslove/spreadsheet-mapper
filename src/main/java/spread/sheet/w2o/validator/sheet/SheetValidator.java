package spread.sheet.w2o.validator.sheet;

import spread.sheet.model.meta.SheetMeta;
import spread.sheet.w2o.validator.Validator;
import spread.sheet.model.core.Sheet;

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
