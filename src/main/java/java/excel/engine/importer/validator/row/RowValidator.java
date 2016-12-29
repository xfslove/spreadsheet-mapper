package java.excel.engine.importer.validator.row;


import java.excel.engine.model.excel.Row;
import java.excel.engine.importer.validator.DataValidator;

/**
 * excel row values validator, after sheet validators.
 * <p>
 * Created by hanwen on 4/26/16.
 */
public interface RowValidator extends DataValidator<Row> {

  /**
   * valid supplied excel row
   *
   * @param row row
   * @return result
   */
  @Override
  boolean valid(Row row);

  /**
   * @return which sheet this validator belong to
   */
  int getSheetIndex();
}
