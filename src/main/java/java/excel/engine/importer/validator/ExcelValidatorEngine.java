package java.excel.engine.importer.validator;

import java.excel.engine.importer.validator.cell.CellValidator;
import java.excel.engine.importer.validator.row.RowValidator;
import java.excel.engine.importer.validator.sheet.SheetValidator;
import java.excel.engine.importer.validator.workbook.WorkbookValidator;

/**
 * excel file validator
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface ExcelValidatorEngine {

  /**
   * @param validators workbook validator
   * @see WorkbookValidator
   */
  void addWorkbookValidator(WorkbookValidator... validators);

  /**
   * @param validators sheet validator
   * @see SheetValidator
   */
  void addSheetValidator(SheetValidator... validators);

  /**
   * <pre>
   * multi validators can hold same key({@link DataValidator#getKey()}),
   * if validators has same key, dependsOn({@link DataValidator#getDependsOn()}) will valid all the validators with same key.
   * </pre>
   *
   * @param validators row validator
   * @see RowValidator
   */
  void addRowValidator(RowValidator... validators);

  /**
   * <pre>
   * multi validators can hold same key({@link DataValidator#getKey()}),
   * if validators has same key, dependsOn({@link DataValidator#getDependsOn()}) will valid all the validators with same key.
   * </pre>
   *
   * @param validators cell validator
   * @see CellValidator
   */
  void addCellValidator(CellValidator... validators);

  /**
   * execute valid
   *
   * @return true if passed all validator
   */
  boolean valid();
}
