package excel.engine.importer.validator.sheet;

import excel.engine.model.excel.Sheet;
import excel.engine.importer.validator.Validator;

/**
 * excel sheet validator, after workbook validators, if workbook validators failure, sheet validators will skip.
 * <p>
 * Created by hanwen on 2016/12/23.
 */
public interface SheetValidator extends Validator<Sheet> {

  /**
   * valid supplied excel sheet
   *
   * @param sheet sheet
   * @return true if pass
   */
  @Override
  boolean valid(Sheet sheet);
}
