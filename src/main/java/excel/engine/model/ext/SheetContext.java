package excel.engine.model.ext;

import excel.engine.model.excel.Sheet;

import java.io.Serializable;
import java.util.List;

/**
 * the context corresponding excel sheet data at {@link #getSheetIndex()}.
 * <p>
 * Created by hanwen on 2016/12/29.
 */
public interface SheetContext extends Serializable {

  /**
   * <pre>
   * add sheet headers, the values using fields({@link #getFields()}) sequence to corresponding.
   * the values length must be equals fields size for correct corresponding.
   * header meta unique with row index in one sheet (one to one),
   * if you add meta with same row index ({@link HeaderMeta#getRowIndex()}),
   * after add will override before add
   * </pre>
   *
   * @param meta   which header
   * @param values header text values
   */
  void addHeader(HeaderMeta meta, String... values);

  /**
   * <pre>
   * add sheet header.
   * header meta unique with row index in one sheet (one to one),
   * if you add meta with same row index ({@link HeaderMeta#getRowIndex()}),
   * after add will override before add
   * </pre>
   *
   * @param sheetHeader header
   */
  void addHeader(SheetHeader sheetHeader);

  /**
   * set the object list data, the data start row index use max header row index plus 1
   *
   * @param data data
   */
  void setData(List<Object> data);

  /**
   * set the object list data and start row index
   *
   * @param dataStartRowIndex data start row index
   * @param data              data
   */
  void setData(int dataStartRowIndex, List<Object> data);

  /**
   * @return sheet index
   * @see Sheet#getIndex()
   */
  int getSheetIndex();

  /**
   * @return sheet name
   * @see Sheet#getName()
   */
  String getSheetName();

  /**
   * the fields sequence present cell sequence in row (include data of field and header of field)
   *
   * @return current context supplied which fields value of data ({@link #getData()})
   */
  List<String> getFields();

  /**
   * if data empty, this get max header row index plus 1
   *
   * @return data start row index
   * @see SheetTemplate#getDataStartRowIndex()
   */
  int getDataStartRowIndex();

  /**
   * @return the object list corresponding one sheet data
   */
  List<Object> getData();

  /**
   * @return sheet header
   * @see SheetHeader
   */
  List<SheetHeader> getSheetHeaders();

  /**
   * @return sheet template
   * @see SheetTemplate
   */
  SheetTemplate ofTemplate();
}
