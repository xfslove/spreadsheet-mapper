package spreadsheet.mapper.m2f.write.strategy;

import org.apache.poi.ss.usermodel.Workbook;
import spreadsheet.mapper.model.msg.Message;

import java.util.Collection;

/**
 * message write strategy
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public interface MessageWriteStrategy {

  /**
   * @return {@link spreadsheet.mapper.model.msg.MessageWriteStrategies}
   */
  String getStrategy();

  /**
   * write messages
   *
   * @param workbook {@link Workbook}
   * @param messages {@link Message}
   */
  void write(Workbook workbook, Collection<Message> messages);
}
