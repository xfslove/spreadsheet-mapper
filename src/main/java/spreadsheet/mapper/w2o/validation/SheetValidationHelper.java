package spreadsheet.mapper.w2o.validation;

import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;
import spreadsheet.mapper.model.msg.ErrorMessage;
import spreadsheet.mapper.model.msg.MessageWriteStrategies;
import spreadsheet.mapper.w2o.validation.validator.DependencyValidator;
import spreadsheet.mapper.w2o.validation.validator.cell.CellValidator;
import spreadsheet.mapper.w2o.validation.validator.row.RowValidator;
import spreadsheet.mapper.w2o.validation.validator.sheet.SheetValidator;

import java.util.List;

/**
 * sheet validation helper
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface SheetValidationHelper {

  /**
   * @param validators {@link SheetValidator}
   * @return {@link SheetValidationHelper}
   */
  SheetValidationHelper sheetValidators(SheetValidator... validators);

  /**
   * @param validators {@link RowValidator}
   * @return {@link SheetValidationHelper}
   * @see DependencyValidator#getGroup()
   * @see DependencyValidator#getDependsOn()
   */
  SheetValidationHelper rowValidators(RowValidator... validators);

  /**
   * @param validators {@link CellValidator}
   * @return {@link SheetValidationHelper}
   * @see DependencyValidator#getGroup()
   * @see DependencyValidator#getDependsOn()
   */
  SheetValidationHelper cellValidators(CellValidator... validators);

  /**
   * @param sheet {@link Sheet}
   * @return {@link SheetValidationHelper}
   */
  SheetValidationHelper sheet(Sheet sheet);

  /**
   * @param sheetMeta {@link SheetMeta}
   * @return {@link SheetValidationHelper}
   */
  SheetValidationHelper sheetMeta(SheetMeta sheetMeta);

  /**
   * execute valid
   *
   * @return true if pass all
   */
  boolean valid();

  /**
   * <pre>
   * message write strategy of {@link SheetValidator#getErrorMessage()} is {@link MessageWriteStrategies#TEXT_BOX}
   * message write strategy of {@link RowValidator#getErrorMessage()} &amp; {@link CellValidator#getErrorMessage()} is {@link MessageWriteStrategies#COMMENT}
   * </pre>
   *
   * @return list of valid error messages
   */
  List<ErrorMessage> getErrorMessages();
}
